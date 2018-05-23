%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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

-module(logger_ssl_formatter).

-export([format/2]).

-define(DEC2HEX(X),
        if ((X) >= 0) andalso ((X) =< 9) -> (X) + $0;
           ((X) >= 10) andalso ((X) =< 15) -> (X) + $a - 10
        end).

%%-------------------------------------------------------------------------
%% External API
%%-------------------------------------------------------------------------
format(#{level:= _Level, msg:= {report, Msg}, meta:= _Meta}, _Config0) ->
     #{direction := Direction,
       protocol := Protocol,
       version := Version,
       message := BinMsg0} = Msg,
    case Protocol of
        'tls_record' ->
            BinMsg = lists:flatten(BinMsg0),
            format_tls_record(Direction, Version, BinMsg);
        'handshake' ->
            [];
        _Other ->
            []
    end.


%%-------------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------------
format_tls_record(Direction, Version, BinMsg) ->
    {Message, Size} = convert_to_hex('tls_record', BinMsg),
    Header = io_lib:format("~s (~B bytes) ~s~n",
                            [header_prefix_tls_record(Direction),
                             Size,
                            tls_record_version(Version, BinMsg)]),
    Header ++ Message.


header_prefix_tls_record(inbound) ->
    "reading";
header_prefix_tls_record(outbound) ->
    "writing".



tls_record_version({3,3}, [<<B,_/binary>>|_]) ->
    io_lib:format("TLS 1.2 Record Protocol, ~s", [msg_type(B)]);
tls_record_version({3,2}, [<<B,_/binary>>|_]) ->
    io_lib:format("TLS 1.1 Record Protocol, ~s", [msg_type(B)]);
tls_record_version({3,1}, [<<B,_/binary>>|_]) ->
    io_lib:format("TLS 1.0 Record Protocol, ~s", [msg_type(B)]);
tls_record_version({3,0}, [<<B,_/binary>>|_]) ->
    io_lib:format("SSL 3.0 Record Protocol, ~s", [msg_type(B)]).

tls_version({3,3}, [<<B,_/binary>>|_]) ->
    io_lib:format("TLS 1.2 Handshake, ~s", [msg_type(B)]);
tls_version({3,2}, [<<B,_/binary>>|_]) ->
    io_lib:format("TLS 1.1 Handshake, ~s", [msg_type(B)]);
tls_version({3,1}, [<<B,_/binary>>|_]) ->
    io_lib:format("TLS 1.0 Handshake, ~s", [msg_type(B)]);
tls_version({3,0}, [<<B,_/binary>>|_]) ->
    io_lib:format("SSL 3.0 Handshake, ~s", [msg_type(B)]).

msg_type(20) -> "change_cipher_spec";
msg_type(21) -> "alert";
msg_type(22) -> "handshake";
msg_type(23) -> "application_data";
msg_type(_) -> unknown.


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


row_prefix(tls_record, N) ->
    S = string:pad(string:to_lower(erlang:integer_to_list(N, 16)),4,leading,$0),
    lists:reverse(lists:flatten(S ++ " - "));
row_prefix(handshake, _) ->
    "   ".


end_row(tls_record, Row) ->
    Row ++ "  ";
end_row(_, Row) ->
    Row.


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
