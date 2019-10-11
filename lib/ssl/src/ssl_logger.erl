%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2019. All Rights Reserved.
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

-module(ssl_logger).

-export([log/4, 
         debug/4,
         format/2,
         format/1]).

-define(DEC2HEX(X),
        if ((X) >= 0) andalso ((X) =< 9) -> (X) + $0;
           ((X) >= 10) andalso ((X) =< 15) -> (X) + $a - 10
        end).

-define(rec_info(T,R),lists:zip(record_info(fields,T),tl(tuple_to_list(R)))).

-include("ssl_internal.hrl").
-include("tls_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("tls_handshake.hrl").
-include("dtls_handshake.hrl").
-include("tls_handshake_1_3.hrl").
-include_lib("kernel/include/logger.hrl").

%%-------------------------------------------------------------------------
%% Internal API -- Stateful logging
%%-------------------------------------------------------------------------

log(Level, LogLevel, ReportMap, Meta) ->
    case logger:compare_levels(LogLevel, Level) of
        lt ->
            logger:log(Level, ReportMap,  Meta#{depth => ?DEPTH, 
                                                report_cb => fun ?MODULE:format/1});
        eq ->
            logger:log(Level, ReportMap, Meta#{depth => ?DEPTH, 
                                               report_cb => fun ?MODULE:format/1});
        _ ->
            ok
    end.

debug(Level, Direction, Protocol, Message)
  when (Direction =:= inbound orelse Direction =:= outbound) andalso
       (Protocol =:= 'record' orelse Protocol =:= 'handshake') ->
    case logger:compare_levels(Level, debug) of
        lt ->
            ?LOG_DEBUG(#{direction => Direction,
                         protocol => Protocol,
                         message => Message},
                       #{domain => [otp,ssl,Protocol]});
        eq ->
            ?LOG_DEBUG(#{direction => Direction,
                         protocol => Protocol,
                         message => Message},
                       #{domain => [otp,ssl,Protocol]});
        _ ->
            ok
    end.

%%-------------------------------------------------------------------------
%%  Report formatting CB
%%-------------------------------------------------------------------------
format(#{alert := Alert, alerter := own} = Report) ->
    #{protocol := ProtocolName,
      role := Role,
      alert := Alert,
      statename := StateName } = Report,
    ssl_alert:own_alert_format(ProtocolName, Role, StateName, Alert);
format(#{alert := Alert, alerter := peer} = Report) ->
    #{protocol := ProtocolName,
      role := Role,
      alert := Alert,
      statename := StateName } = Report,
    ssl_alert:alert_format(ProtocolName, Role, StateName, Alert);
format(#{alert := Alert, alerter := ignored} = Report) -> 
    #{protocol := ProtocolName,
      role := Role,
      alert := Alert,
      statename := StateName} = Report,
    %% Happens in DTLS
    {Fmt, Args} = ssl_alert:own_alert_format(ProtocolName, Role, StateName, Alert),
    {"~s " ++ Fmt, ["Ignored alert to mitigate DoS attacks", Args]};
format(#{description := Desc} = Report) ->
    #{reason := Reason}  = Report,
    {"~s11:~p"
    "~n"
     "~s11:~p"
    "~n",
     ["Description", Desc, "Reason", Reason]
    }.

%%-------------------------------------------------------------------------
%%  SSL log handler formatter
%%-------------------------------------------------------------------------
format(#{msg:= {report, Msg}}, _Config0) ->
     #{direction := Direction,
       protocol := Protocol,
       message := Content} = Msg,
    case Protocol of
        'record' ->
            BinMsg =
                case Content of
                    #ssl_tls{} ->
                        [tls_record:build_tls_record(Content)];
                    _ when is_list(Content) ->
                        lists:flatten(Content)
                end,
            format_tls_record(Direction, BinMsg);
        'handshake' ->
            format_handshake(Direction, Content);
        _Other ->
            []
    end.

%%-------------------------------------------------------------------------
%%  Internal functions
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Handshake Protocol
%%-------------------------------------------------------------------------
format_handshake(Direction, BinMsg) ->
    {Header, Message} = parse_handshake(Direction, BinMsg),
    io_lib:format("~s~n~s~n", [Header, Message]).


parse_handshake(Direction, #client_hello{
                              client_version = Version0,
                              cipher_suites = CipherSuites0,
                              extensions = Extensions
                             } = ClientHello) ->
    Version = get_client_version(Version0, Extensions),
    Header = io_lib:format("~s ~s Handshake, ClientHello",
                           [header_prefix(Direction),
                            version(Version)]),
    CipherSuites = parse_cipher_suites(CipherSuites0),
    Message = io_lib:format("~p",
                            [?rec_info(client_hello,
                                       ClientHello#client_hello{cipher_suites = CipherSuites})]),
    {Header, Message};
parse_handshake(Direction, #server_hello{
                              server_version = Version0,
                              cipher_suite = CipherSuite0,
                              extensions = Extensions
                             } = ServerHello) ->
    Version = get_server_version(Version0, Extensions),
    Header = io_lib:format("~s ~s Handshake, ServerHello",
                           [header_prefix(Direction),
                            version(Version)]),
    CipherSuite = format_cipher(CipherSuite0),
    Message = io_lib:format("~p",
                            [?rec_info(server_hello,
                                       ServerHello#server_hello{cipher_suite = CipherSuite})]),
    {Header, Message};
parse_handshake(Direction, #hello_verify_request{} = HelloVerifyRequest) ->
    Header = io_lib:format("~s Handshake, HelloVerifyRequest",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(hello_verify_request, HelloVerifyRequest)]),
    {Header, Message};
parse_handshake(Direction, #certificate{} = Certificate) ->
    Header = io_lib:format("~s Handshake, Certificate",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(certificate, Certificate)]),
    {Header, Message};
parse_handshake(Direction, #server_key_exchange{} = ServerKeyExchange) ->
    Header = io_lib:format("~s Handshake, ServerKeyExchange",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(server_key_exchange, ServerKeyExchange)]),
    {Header, Message};
parse_handshake(Direction, #server_key_params{} = ServerKeyExchange) ->
    Header = io_lib:format("~s Handshake, ServerKeyExchange",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(server_key_params, ServerKeyExchange)]),
    {Header, Message};
parse_handshake(Direction, #certificate_request{} = CertificateRequest) ->
    Header = io_lib:format("~s Handshake, CertificateRequest",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(certificate_request, CertificateRequest)]),
    {Header, Message};
parse_handshake(Direction, #server_hello_done{} = ServerHelloDone) ->
    Header = io_lib:format("~s Handshake, ServerHelloDone",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(server_hello_done, ServerHelloDone)]),
    {Header, Message};
parse_handshake(Direction, #client_key_exchange{} = ClientKeyExchange) ->
    Header = io_lib:format("~s Handshake, ClientKeyExchange",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(client_key_exchange, ClientKeyExchange)]),
    {Header, Message};
parse_handshake(Direction, #certificate_verify{} = CertificateVerify) ->
    Header = io_lib:format("~s Handshake, CertificateVerify",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(certificate_verify, CertificateVerify)]),
    {Header, Message};
parse_handshake(Direction, #finished{} = Finished) ->
    Header = io_lib:format("~s Handshake, Finished",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(finished, Finished)]),
    {Header, Message};
parse_handshake(Direction, #hello_request{} = HelloRequest) ->
    Header = io_lib:format("~s Handshake, HelloRequest",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(hello_request, HelloRequest)]),
    {Header, Message};
parse_handshake(Direction, #certificate_request_1_3{} = CertificateRequest) ->
    Header = io_lib:format("~s Handshake, CertificateRequest",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(certificate_request_1_3, CertificateRequest)]),
    {Header, Message};
parse_handshake(Direction, #certificate_1_3{} = Certificate) ->
    Header = io_lib:format("~s Handshake, Certificate",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(certificate_1_3, Certificate)]),
    {Header, Message};
parse_handshake(Direction, #certificate_verify_1_3{} = CertificateVerify) ->
    Header = io_lib:format("~s Handshake, CertificateVerify",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(certificate_verify_1_3, CertificateVerify)]),
    {Header, Message};
parse_handshake(Direction, #encrypted_extensions{} = EncryptedExtensions) ->
    Header = io_lib:format("~s Handshake, EncryptedExtensions",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(encrypted_extensions, EncryptedExtensions)]),
    {Header, Message};
parse_handshake(Direction, #new_session_ticket{} = NewSessionTicket) ->
    Header = io_lib:format("~s Post-Handshake, NewSessionTicket",
                           [header_prefix(Direction)]),
    Message = io_lib:format("~p", [?rec_info(new_session_ticket, NewSessionTicket)]),
    {Header, Message}.


parse_cipher_suites([_|_] = Ciphers) ->
    [format_cipher(C) || C <- Ciphers].

format_cipher(C0) ->
    try ssl_cipher_format:suite_bin_to_map(C0) of
        Map ->
            ssl_cipher_format:suite_map_to_str(Map)
    catch 
        error:function_clause ->
            format_uknown_cipher_suite(C0)
    end.

get_client_version(Version, Extensions) ->
    CHVersions = maps:get(client_hello_versions, Extensions, undefined),
    case CHVersions of
        #client_hello_versions{versions = [Highest|_]} ->
            Highest;
        undefined ->
            Version
    end.

get_server_version(Version, Extensions) ->
    SHVersion = maps:get(server_hello_selected_version, Extensions, undefined),
    case SHVersion of
        #server_hello_selected_version{selected_version = SelectedVersion} ->
            SelectedVersion;
        undefined ->
            Version
    end.

version({3,4}) ->
    "TLS 1.3";
version({3,3}) ->
    "TLS 1.2";
version({3,2}) ->
    "TLS 1.1";
version({3,1}) ->
    "TLS 1.0";
version({3,0}) ->
    "SSL 3.0";
version({254,253}) ->
    "DTLS 1.2";
version({254,255}) ->
    "DTLS 1.0";
version({M,N}) ->
    io_lib:format("TLS/DTLS [0x0~B0~B]", [M,N]).

header_prefix(inbound) ->
    "<<<";
header_prefix(outbound) ->
    ">>>".


%%-------------------------------------------------------------------------
%% TLS Record Protocol
%%-------------------------------------------------------------------------
format_tls_record(Direction, BinMsg) ->
    {Message, Size} = convert_to_hex('tls_record', BinMsg),
    Header = io_lib:format("~s (~B bytes) ~s~n",
                           [header_prefix_tls_record(Direction),
                            Size,
                            tls_record_version(BinMsg)]),
    Header ++ Message.


header_prefix_tls_record(inbound) ->
    "reading";
header_prefix_tls_record(outbound) ->
    "writing".


tls_record_version([<<?BYTE(B),?BYTE(3),?BYTE(3),_/binary>>|_]) ->
    io_lib:format("TLS 1.2 Record Protocol, ~s", [msg_type(B)]);
tls_record_version([<<?BYTE(B),?BYTE(3),?BYTE(2),_/binary>>|_]) ->
    io_lib:format("TLS 1.1 Record Protocol, ~s", [msg_type(B)]);
tls_record_version([<<?BYTE(B),?BYTE(3),?BYTE(1),_/binary>>|_]) ->
    io_lib:format("TLS 1.0 Record Protocol, ~s", [msg_type(B)]);
tls_record_version([<<?BYTE(B),?BYTE(3),?BYTE(0),_/binary>>|_]) ->
    io_lib:format("SSL 3.0 Record Protocol, ~s", [msg_type(B)]);
tls_record_version([<<?BYTE(B),?BYTE(254),?BYTE(253),_/binary>>|_]) ->
    io_lib:format("DTLS 1.2 Record Protocol, ~s", [msg_type(B)]);
tls_record_version([<<?BYTE(B),?BYTE(254),?BYTE(255),_/binary>>|_]) ->
    io_lib:format("DTLS 1.0 Record Protocol, ~s", [msg_type(B)]);
tls_record_version([<<?BYTE(B),?BYTE(M),?BYTE(N),_/binary>>|_]) ->
    io_lib:format("TLS/DTLS [0x0~B0~B] Record Protocol, ~s", [M, N, msg_type(B)]).


msg_type(20) -> "change_cipher_spec";
msg_type(21) -> "alert";
msg_type(22) -> "handshake";
msg_type(23) -> "application_data";
msg_type(_) -> unknown.


%%-------------------------------------------------------------------------
%% Hex encoding functions
%%-------------------------------------------------------------------------
convert_to_hex(Protocol, BinMsg) ->
    convert_to_hex(Protocol, BinMsg, [], [], 0).
%%
convert_to_hex(P, [], Row0, Acc, C) when C rem 16 =:= 0 ->
    Row = lists:reverse(end_row(P, Row0)),
    {lists:reverse(Acc) ++ Row ++ io_lib:nl(), C};
convert_to_hex(P, [], Row0, Acc, C) ->
    Row = lists:reverse(end_row(P, Row0)),
    Padding = calculate_padding(Row0, Acc),
    PaddedRow = string:pad(Row, Padding, leading, $ ),
    {lists:reverse(Acc) ++ PaddedRow ++ io_lib:nl(), C};
convert_to_hex(P, [H|T], Row, Acc, C) when is_list(H) ->
    convert_to_hex(P, H ++ T, Row, Acc, C);
convert_to_hex(P, [<<>>|T], Row, Acc, C) ->
    convert_to_hex(P, T, Row, Acc, C);

%% First line
convert_to_hex(P, [<<A:4,B:4,R/binary>>|T], Row, Acc, C) when C =:= 0 ->
    convert_to_hex(P, [<<R/binary>>|T],
                   update_row(<<A:4,B:4>>, Row),
                   prepend_first_row(P, A, B, Acc, C),
                   C + 1);
%% New line
convert_to_hex(P, [<<A:4,B:4,R/binary>>|T], Row, Acc, C) when C rem 16 =:= 0 ->
    convert_to_hex(P, [<<R/binary>>|T],
                   update_row(<<A:4,B:4>>, []),
                   prepend_row(P, A, B, Row, Acc, C),
                   C + 1);
%% Add 8th hex with extra whitespace
%% 0000 - 16 03 02 00 bd 01 00 00  b9 ...
%%                             ^^^^
convert_to_hex(P, [<<A:4,B:4,R/binary>>|T], Row, Acc, C) when C rem 8 =:= 7 ->
    convert_to_hex(P, [<<R/binary>>|T],
                   update_row(<<A:4,B:4>>, Row),
                   prepend_eighths_hex(A, B, Acc),
                   C + 1);
convert_to_hex(P, [<<A:4,B:4,R/binary>>|T], Row, Acc, C) ->
    convert_to_hex(P, [<<R/binary>>|T],
                   update_row(<<A:4,B:4>>, Row),
                   prepend_hex(A, B, Acc),
                   C + 1);
%% First line
convert_to_hex(P, [H|T], Row, Acc, C) when is_integer(H), C =:= 0 ->
    convert_to_hex(P, T,
                   update_row(H, Row),
                   prepend_first_row(P, H, Acc, C),
                   C + 1);
%% New line
convert_to_hex(P, [H|T], Row, Acc, C) when is_integer(H), C rem 16 =:= 0 ->
    convert_to_hex(P, T,
                   update_row(H, []),
                   prepend_row(P, H, Row, Acc, C),
                   C + 1);
%% Add 8th hex with extra whitespace
%% 0000 - 16 03 02 00 bd 01 00 00  b9 ...
%%                             ^^^^
convert_to_hex(P, [H|T], Row, Acc, C) when is_integer(H), C rem 8 =:= 7 ->
    convert_to_hex(P, T,
                   update_row(H, Row),
                   prepend_eighths_hex(H, Acc),
                   C + 1);
convert_to_hex(P, [H|T], Row, Acc, C) when is_integer(H) ->
    convert_to_hex(P, T,
                   update_row(H, Row),
                   prepend_hex(H, Acc),
                   C + 1).


row_prefix(_ , N) ->
    S = string:pad(string:to_lower(erlang:integer_to_list(N, 16)),4,leading,$0),
    lists:reverse(lists:flatten(S ++ " - ")).


end_row(_, Row) ->
    Row ++ "  ".


%% Calculate padding of the "printable character" lines in order to be
%% visually aligned.
calculate_padding(Row, Acc) ->
    %% Number of new line characters
    NNL = (length(Acc) div 75) * length(io_lib:nl()),
    %% Length of the last printed line
    Length = (length(Acc) - NNL) rem 75,
    %% Adjusted length of the last printed line
    PaddedLength = 75 - (16 - length(Row)), %% Length
    %% Padding
    PaddedLength - Length.


%%-------------------------------------------------------------------------
%% Functions operating on reversed lists
%%-------------------------------------------------------------------------
update_row(B, Row) when is_binary(B) ->
    case binary_to_list(B) of
        [C] when 32 =< C, C =< 126 ->
            [C|Row];
        _Else ->
            [$.|Row]
    end;
update_row(C, Row) when 32 =< C, C =< 126 ->
    [C|Row];
update_row(_, Row) ->
    [$.|Row].


prepend_first_row(P, A, B, Acc, C) ->
    prepend_hex(A, B,row_prefix(P, C) ++ Acc).
%%
prepend_first_row(P, N, Acc, C) ->
    prepend_hex(N,row_prefix(P, C) ++ Acc).

prepend_row(P, A, B, Row, Acc, C) ->
    prepend_hex(A, B,row_prefix(P, C) ++ io_lib:nl() ++ end_row(P, Row) ++ Acc).
%%
prepend_row(P, N, Row, Acc, C) ->
    prepend_hex(N,row_prefix(P, C) ++ io_lib:nl() ++ end_row(P, Row) ++ Acc).



prepend_hex(A, B, Acc) ->
    [$ ,?DEC2HEX(B),?DEC2HEX(A)|Acc].
%%
prepend_hex(N, Acc) ->
    " " ++ number_to_hex(N) ++ Acc.


prepend_eighths_hex(A, B, Acc) ->
    [$ ,$ ,?DEC2HEX(B),?DEC2HEX(A)|Acc].
%%
prepend_eighths_hex(N, Acc) ->
    "  " ++ number_to_hex(N) ++ Acc.

number_to_hex(N) ->
    case string:to_lower(erlang:integer_to_list(N, 16)) of
        H when length(H) < 2 ->
            lists:append(H, "0");
        H ->
            lists:reverse(H)
    end.
 
format_uknown_cipher_suite(<<?BYTE(X), ?BYTE(Y)>>) ->
    "0x" ++ number_to_hex(X) ++ "0x" ++ number_to_hex(Y).

