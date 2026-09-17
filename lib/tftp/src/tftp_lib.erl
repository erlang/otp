%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2005-2026. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : tftp_lib.erl
%%% Author  : Hakan Mattsson <hakan@erix.ericsson.se>
%%% Description : Option parsing, decode, encode etc.
%%%
%%% Created : 18 May 2004 by Hakan Mattsson <hakan@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(tftp_lib).
-moduledoc false.

%%-------------------------------------------------------------------
%% Interface
%%-------------------------------------------------------------------

%% application internal functions
-export([
         parse_config/1,
         parse_config/2,
         decode_msg/1,
         encode_msg/1,
         replace_val_r/3,
         to_lower/1,
         host_to_string/1,
	 add_default_callbacks/1
        ]).

%%-------------------------------------------------------------------
%% Defines
%%-------------------------------------------------------------------

-include("tftp.hrl").

-define(LOWER(Char),
        if
            Char >= $A, Char =< $Z ->
                Char - ($A - $a);
            true ->
                Char
        end).

%%-------------------------------------------------------------------
%% Config
%%-------------------------------------------------------------------

parse_config(Options) ->
    parse_config(Options, #config{}).

parse_config(Options, #config{udp_options = UdpOptions, user_options = TftpOptions} = Config) ->
    do_parse_config(Options, Config, UdpOptions, TftpOptions).

do_parse_config([{Key, Val} | Tail], #config{} = Config, UdpOptions, TftpOptions) ->
    case Key of
        debug ->
            if
                Val =:= 0; Val =:= none ->
                    do_parse_config(Tail, Config#config{debug_level = none}, UdpOptions, TftpOptions);
                Val =:= 1; Val =:= error ->
                    do_parse_config(Tail, Config#config{debug_level = error}, UdpOptions, TftpOptions);
                Val =:= 2; Val =:= warning ->
                    do_parse_config(Tail, Config#config{debug_level = warning}, UdpOptions, TftpOptions);
                Val =:= 3; Val =:= brief ->
                    do_parse_config(Tail, Config#config{debug_level = brief}, UdpOptions, TftpOptions);
                Val =:= 4; Val =:= normal ->
                    do_parse_config(Tail, Config#config{debug_level = normal}, UdpOptions, TftpOptions);
                Val =:= 5; Val =:= verbose ->
                    do_parse_config(Tail, Config#config{debug_level = verbose}, UdpOptions, TftpOptions);
                Val =:= 6; Val =:= all ->
                    do_parse_config(Tail, Config#config{debug_level = all}, UdpOptions, TftpOptions);
                true ->
                    throw({badarg, {Key, Val}})
            end;
        host ->
            if
                is_list(Val) ->
                    do_parse_config(Tail, Config#config{udp_host = Val}, UdpOptions, TftpOptions);
                tuple_size(Val) =:= 4 ->
                    do_parse_config(Tail, Config#config{udp_host = Val}, UdpOptions, TftpOptions);
                tuple_size(Val) =:= 8 ->
                    do_parse_config(Tail, Config#config{udp_host = Val}, UdpOptions, TftpOptions);
                true ->
                    throw({badarg, {Key, Val}})
            end;
        port ->
            if
                is_integer(Val), Val >= 0 ->
                    Config2 = Config#config{udp_port = Val},
                    do_parse_config(Tail, Config2, UdpOptions, TftpOptions);
                true ->
                    throw({badarg, {Key, Val}})
            end;
        port_policy ->
            case Val of
                random ->
                    do_parse_config(Tail, Config#config{port_policy = Val}, UdpOptions, TftpOptions);
                0 ->
                    do_parse_config(Tail, Config#config{port_policy = random}, UdpOptions, TftpOptions);
                MinMax when is_integer(MinMax), MinMax > 0 ->
                    do_parse_config(Tail, Config#config{port_policy = {range, MinMax, MinMax}}, UdpOptions, TftpOptions);
                {range, Min, Max} when Max >= Min,
                is_integer(Min), Min > 0,
                is_integer(Max), Max > 0 ->
                    do_parse_config(Tail, Config#config{port_policy = Val}, UdpOptions, TftpOptions);
                _ ->
                    throw({badarg, {Key, Val}})
            end;
        udp when is_list(Val) ->
            Fun =
                fun({K, V}, List) when K /= active ->
                        replace_val_r(K, V, List);
                   (V, List) when V /= list, V /= binary ->
                        List ++ [V];
                   (V, _List) ->
                        throw({badarg, {udp, [V]}})
                end,
            NewUdpOptions = lists:foldl(Fun, UdpOptions, Val),
            do_parse_config(Tail, Config, NewUdpOptions, TftpOptions);
        use_tsize ->
            case Val of
                true ->
                    do_parse_config(Tail, Config#config{use_tsize = Val}, UdpOptions, TftpOptions);
                false ->
                    do_parse_config(Tail, Config#config{use_tsize = Val}, UdpOptions, TftpOptions);
                _ ->
                    throw({badarg, {Key, Val}})
            end;
        max_tsize ->
            if
                Val =:= infinity ->
                    do_parse_config(Tail, Config#config{max_tsize = Val}, UdpOptions, TftpOptions);
                is_integer(Val), Val >= 0 ->
                    do_parse_config(Tail, Config#config{max_tsize = Val}, UdpOptions, TftpOptions);
                true ->
                    throw({badarg, {Key, Val}})
            end;
        max_conn ->
            if
                Val =:= infinity ->
                    do_parse_config(Tail, Config#config{max_conn = Val}, UdpOptions, TftpOptions);
                is_integer(Val), Val > 0 ->
                    do_parse_config(Tail, Config#config{max_conn = Val}, UdpOptions, TftpOptions);
                true ->
                    throw({badarg, {Key, Val}})
            end;
        _ when is_list(Key), is_list(Val) ->
            Key2 = to_lower(Key),
            Val2 = to_lower(Val),
            NewTftpOptions = replace_val_r(Key2, Val2, TftpOptions),
            do_parse_config(Tail, Config, UdpOptions, NewTftpOptions);
        reject ->
            case Val of
                read ->
                    Rejected = [Val | Config#config.rejected],
                    do_parse_config(Tail, Config#config{rejected = Rejected}, UdpOptions, TftpOptions);
                write ->
                    Rejected = [Val | Config#config.rejected],
                    do_parse_config(Tail, Config#config{rejected = Rejected}, UdpOptions, TftpOptions);
                _ when is_list(Val) ->
                    Rejected = [Val | Config#config.rejected],
                    do_parse_config(Tail, Config#config{rejected = Rejected}, UdpOptions, TftpOptions);
                _ ->
                    throw({badarg, {Key, Val}})
            end;
        callback ->
            case Val of
                {RegExp, Mod, State} when is_list(RegExp), is_atom(Mod) ->
                    case re:compile(RegExp) of
                        {ok, Internal} ->
                            Callback = #callback{regexp   = RegExp,
                                                 internal = Internal,
                                                 module   = Mod,
                                                 state    = State},
                            Callbacks = Config#config.callbacks ++ [Callback],
                            do_parse_config(Tail, Config#config{callbacks = Callbacks}, UdpOptions, TftpOptions);
                        {error, Reason} ->
                            throw({badarg, {Key, Val}, Reason})
                    end;
                _ ->
                    throw({badarg, {Key, Val}})
            end;
        logger ->
            if
                is_atom(Val) ->
                    do_parse_config(Tail, Config#config{logger = Val}, UdpOptions, TftpOptions);
                true ->
                    throw({badarg, {Key, Val}})
            end;
        max_retries ->
            if
                is_integer(Val), Val >= 0 ->
                    do_parse_config(Tail, Config#config{max_retries = Val}, UdpOptions, TftpOptions);
                true ->
                    throw({badarg, {Key, Val}})
            end;
        _ ->
            throw({badarg, {Key, Val}})
    end;
do_parse_config([], #config{udp_host     = Host,
                            callbacks    = Callbacks} = Config,
                UdpOptions, TftpOptions) ->
    IsInet6 = lists:member(inet6, UdpOptions),
    IsInet  = lists:member(inet, UdpOptions),
    Host2 =
        if
            IsInet, not IsInet6;
            not IsInet, not IsInet6 ->
                case inet:getaddr(Host, inet) of
                    {ok, Addr} ->
                        Addr;
                    {error, Reason} ->
                        throw({badarg, {host, Reason}})
                end;
            IsInet6, not IsInet  ->
                case inet:getaddr(Host, inet6) of
                    {ok, Addr} ->
                        Addr;
                    {error, Reason} ->
                        throw({badarg, {host, Reason}})
                end;
            true ->
                %% Conflicting options
                throw({badarg, {udp, [inet]}})
        end,
    Callbacks2  = add_default_callbacks(Callbacks),
    Config#config{udp_host     = Host2,
                  udp_options  = UdpOptions,
                  user_options = TftpOptions,
                  callbacks    = Callbacks2};
do_parse_config(Options, #config{}, _UdpOptions, _TftpOptions) ->
    throw({badarg, Options}).

add_default_callbacks(Callbacks) ->
    RegExp = "",
    {ok, Internal} = re:compile(RegExp),
    File = #callback{regexp   = RegExp,
		     internal = Internal,
		     module   = tftp_file,
		     state    = []},
    Bin = #callback{regexp   = RegExp,
		    internal = Internal,
		    module   = tftp_binary,
		    state    = []},
    Callbacks ++ [File, Bin].

host_to_string(Host) ->
    case Host of
        String when is_list(String) ->
            String;
        Address when
              tuple_size(Address) =:= 4;
              tuple_size(Address) =:= 8 ->
            inet:ntoa(Address)
    end.

%%-------------------------------------------------------------------
%% Decode
%%-------------------------------------------------------------------

decode_msg(Bin) when is_binary(Bin) ->
    try do_decode_msg(Bin)
    catch throw : Text ->
            #tftp_decode_error{reply = #tftp_msg_error{code = undef, text = Text}}
    end.

do_decode_msg(Bin) ->
    case Bin of
        <<?TFTP_OPCODE_RRQ:16/integer, Tail/binary>> ->
            case decode_strings(Tail, [keep_case, lower_case]) of
                [Filename, Mode | Strings] ->
                    Options = decode_options(Strings),
                    #tftp_msg_req{access = read,
                                  filename = Filename,
                                  mode = to_lower(Mode),
                                  options = Options};
                [_Filename | _Strings] ->
                    throw("Missing mode");
                _ ->
                    throw("Missing filename")
            end;
        <<?TFTP_OPCODE_WRQ:16/integer, Tail/binary>> ->
            case decode_strings(Tail, [keep_case, lower_case]) of
                [Filename, Mode | Strings] ->
                    Options = decode_options(Strings),
                    #tftp_msg_req{access = write,
                                  filename = Filename,
                                  mode = to_lower(Mode),
                                  options = Options};
                [_Filename | _Strings] ->
                    throw("Missing mode");
                _ ->
                    throw("Missing filename")
            end;
        <<?TFTP_OPCODE_DATA:16/integer, SeqNo:16/integer, Data/binary>> ->
            #tftp_msg_data{block_no = SeqNo, data = Data};
        <<?TFTP_OPCODE_ACK:16/integer, SeqNo:16/integer>> ->
            #tftp_msg_ack{block_no = SeqNo};
        <<?TFTP_OPCODE_ERROR:16/integer, ErrorCode:16/integer, Tail/binary>> ->
            case decode_strings(Tail, [keep_case]) of
                [ErrorText] ->
                    ErrorCode2 = decode_error_code(ErrorCode),
                    #tftp_msg_error{code = ErrorCode2,
                                    text = ErrorText};
                _ ->
                    throw("Trailing garbage")
            end;
        <<?TFTP_OPCODE_OACK:16/integer, Tail/binary>> ->
            Strings = decode_strings(Tail, [lower_case]),
            Options = decode_options(Strings),
            #tftp_msg_oack{options = Options};
        _ ->
            throw("Invalid syntax")
    end.

decode_strings(Bin, Cases) when is_binary(Bin), is_list(Cases) ->
    do_decode_strings(Bin, Cases, []).

do_decode_strings(<<>>, _Cases, Strings) ->
    lists:reverse(Strings);
do_decode_strings(Bin, [Case | Cases], Strings) ->
    {String, Tail} = decode_string(Bin, Case, []),
    if
        Cases =:= [] ->
            do_decode_strings(Tail, [Case], [String | Strings]);
        true ->
            do_decode_strings(Tail, Cases,  [String | Strings])
    end.

decode_string(<<Char:8/integer, Tail/binary>>, Case, String) ->
    if
        Char =:= 0 ->
            {lists:reverse(String), Tail};
        Case =:= keep_case ->
            decode_string(Tail, Case, [Char | String]);
        Case =:= lower_case ->
            Char2 = ?LOWER(Char),
            decode_string(Tail, Case, [Char2 | String])
    end;
decode_string(<<>>, _Case, _String) ->
    throw("Trailing null missing").

decode_options([Key, Value | Strings]) ->
    [{to_lower(Key), Value} | decode_options(Strings)];
decode_options([_Key]) ->
    throw("Missing option value");
decode_options([]) ->
    [].

decode_error_code(Int) ->
    case Int of
        ?TFTP_ERROR_UNDEF   -> undef;
        ?TFTP_ERROR_ENOENT  -> enoent;
        ?TFTP_ERROR_EACCES  -> eacces;
        ?TFTP_ERROR_ENOSPC  -> enospc;
        ?TFTP_ERROR_BADOP   -> badop;
        ?TFTP_ERROR_BADBLK  -> badblk;
        ?TFTP_ERROR_EEXIST  -> eexist;
        ?TFTP_ERROR_BADUSER -> baduser;
        ?TFTP_ERROR_BADOPT  -> badopt;
        Int when is_integer(Int), Int >= 0, Int =< 65535 -> Int;
        _ -> throw("Error code outside range.")
    end.

%%-------------------------------------------------------------------
%% Encode
%%-------------------------------------------------------------------

encode_msg(#tftp_msg_req{access = Access,
                         filename = Filename,
                         mode = Mode, 
                         options = Options}) ->
    OpCode = case Access of
                 read  -> ?TFTP_OPCODE_RRQ;
                 write -> ?TFTP_OPCODE_WRQ
             end,
    [
     <<OpCode:16/integer>>,
     Filename, 
     0, 
     Mode, 
     0,
     [[Key, 0, Val, 0] || {Key, Val} <- Options]
    ];
encode_msg(#tftp_msg_data{block_no = BlockNo, data = Data}) when BlockNo =< 65535 ->
    [
     <<?TFTP_OPCODE_DATA:16/integer, BlockNo:16/integer>>,
     Data
    ];
encode_msg(#tftp_msg_ack{block_no = BlockNo}) when BlockNo =< 65535 ->
    <<?TFTP_OPCODE_ACK:16/integer, BlockNo:16/integer>>;
encode_msg(#tftp_msg_error{code = Code, text = Text}) ->
    IntCode = encode_error_code(Code),
    [
     <<?TFTP_OPCODE_ERROR:16/integer, IntCode:16/integer>>, 
     Text,
     0
    ];
encode_msg(#tftp_msg_oack{options = Options}) ->
    [
     <<?TFTP_OPCODE_OACK:16/integer>>,
     [[Key, 0, Val, 0] || {Key, Val} <- Options]
    ].

encode_error_code(Code) ->
    case Code of
        undef   -> ?TFTP_ERROR_UNDEF;
        enoent  -> ?TFTP_ERROR_ENOENT;
        eacces  -> ?TFTP_ERROR_EACCES;
        enospc  -> ?TFTP_ERROR_ENOSPC;
        badop   -> ?TFTP_ERROR_BADOP;
        badblk  -> ?TFTP_ERROR_BADBLK;
        eexist  -> ?TFTP_ERROR_EEXIST;
        baduser -> ?TFTP_ERROR_BADUSER;
        badopt  -> ?TFTP_ERROR_BADOPT;
        Int when is_integer(Int), Int >= 0, Int =< 65535 -> Int
    end.

%%-------------------------------------------------------------------
%% Miscellaneous
%%-------------------------------------------------------------------

%% Replace the first occurence of {Key,_}, or if it is not in the option list
%% - prepend it to the list.
replace_val_r(Key, Val, List) when is_list(List) ->
    case do_replace_val_r(Key, Val, List) of
        ReplacedList when is_list(ReplacedList)         -> ReplacedList;
        found                                           -> List;
        not_found                                       -> [{Key, Val} | List]
    end.

do_replace_val_r(_Key, _Val, [])                        -> not_found;
do_replace_val_r(Key, Val, [Item | List]) ->
    if  element(1, Item) == Key ->
            case Item of
                {_, Val}                                -> found;
                {_, _}                                  -> [{Key, Val} | List]
            end;
        true ->
            case do_replace_val_r(Key, Val, List) of
                ReplacedList when is_list(ReplacedList) -> [Item | ReplacedList];
                Result       when is_atom(Result)       -> Result
            end
    end.


to_lower(Chars) ->
    [?LOWER(Char) || Char <- Chars].
