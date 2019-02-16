%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

-module(diameter_codec).

-export([encode/2, encode/3,
         decode/2, decode/3, decode/4,
         collect_avps/1,
         decode_header/1,
         sequence_numbers/1,
         hop_by_hop_id/2,
         msg_name/2,
         msg_id/1]).

%% towards diameter_gen
-export([pack_data/2,
         pack_avp/2]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").

-define(PAD(Len), ((4 - (Len rem 4)) rem 4)).
-define(BIT(B,I), (if B -> I; true -> 0 end)).
-define(BIT(B),   ?BIT(B,1)).
-define(FLAGS(R,P,E,T), ?BIT(R):1, ?BIT(P):1, ?BIT(E):1, ?BIT(T):1, 0:4).
-define(FLAG(B,D), (if is_boolean(B) -> B; true -> 0 /= (D) end)).

-type u32() :: 0..16#FFFFFFFF.
-type u24() :: 0..16#FFFFFF.
-type u1()  :: 0..1.

%%     0                   1                   2                   3
%%     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |    Version    |                 Message Length                |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    | command flags |                  Command-Code                 |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |                         Application-ID                        |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |                      Hop-by-Hop Identifier                    |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |                      End-to-End Identifier                    |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |  AVPs ...
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-

%%% ---------------------------------------------------------------------------
%%% # encode/2
%%% ---------------------------------------------------------------------------

%% The representative encode documented in diameter_codec(3). As of
%% the options that affect encode (eg. ordered_encode), it's no longer
%% *the* encode.

encode(Mod, Msg) ->
    encode(Mod, #{ordered_encode => true}, Msg).

%%% ---------------------------------------------------------------------------
%%% # encode/3
%%% ---------------------------------------------------------------------------

-spec encode(module(),
             map(),
             Msg :: term())
   -> #diameter_packet{}
    | no_return().

encode(Mod, Opts, #diameter_packet{} = Pkt) ->
    try
        enc(Mod, Opts, Pkt)
    catch
        exit: {Reason, Stack, #diameter_header{} = H} = T ->
            %% Exit with a header in the reason to let the caller
            %% count encode errors.
            ?LOG(encode_error, {Reason, Stack, H}),
            exit({?MODULE, encode, T});
        error: Reason: Stack ->
            T = {Reason, diameter_lib:stacktrace(Stack)},
            ?LOG(encode_error, T),
            exit({?MODULE, encode, T})
    end;

encode(Mod, Opts, Msg) ->
    Seq = diameter_session:sequence(),
    Hdr = #diameter_header{version = ?DIAMETER_VERSION,
                           end_to_end_id = Seq,
                           hop_by_hop_id = Seq},
    encode(Mod, Opts, #diameter_packet{header = Hdr,
                                       msg = Msg}).

%% enc/3

enc(_, Opts, #diameter_packet{msg = [#diameter_header{} = Hdr | As]}
             = Pkt) ->
    try encode_avps(As, Opts) of
        Avps ->
            Bin = list_to_binary(Avps),
            Len = 20 + size(Bin),

            #diameter_header{version = Vsn,
                             is_request = R,
                             is_proxiable = P,
                             is_error = E,
                             is_retransmitted = T,
                             cmd_code = Code,
                             application_id = Aid,
                             hop_by_hop_id  = Hid,
                             end_to_end_id  = Eid}
                = Hdr,

            Pkt#diameter_packet{header = Hdr,
                                bin = <<Vsn:8, Len:24,
                                        ?FLAGS(R,P,E,T), Code:24,
                                        Aid:32,
                                        Hid:32,
                                        Eid:32,
                                        Bin/binary>>}
    catch
        error: Reason: Stack ->
            exit({Reason, diameter_lib:stacktrace(Stack), Hdr})
    end;

enc(Mod, Opts, #diameter_packet{header = Hdr0, msg = Msg} = Pkt) ->
    MsgName = rec2msg(Mod, Msg),
    {Code, Flags, Aid} = msg_header(Mod, MsgName, Hdr0),

    #diameter_header{version = Vsn,
                     is_request = R,
                     is_proxiable = P,
                     is_error = E,
                     is_retransmitted = T,
                     hop_by_hop_id = Hid,
                     end_to_end_id = Eid}
        = Hdr0,

    RB = ?FLAG(R, Flags band 2#10000000),
    PB = ?FLAG(P, Flags band 2#01000000),
    EB = ?FLAG(E, Flags band 2#00100000),
    TB = ?FLAG(T, Flags band 2#00010000),

    Values = values(Msg),

    try encode_avps(Mod, MsgName, Values, Opts) of
        Avps ->
            Bin = list_to_binary(Avps),
            Len = 20 + size(Bin),

            Hdr = Hdr0#diameter_header{length = Len,
                                       cmd_code = Code,
                                       application_id = Aid,
                                       is_request       = RB,
                                       is_proxiable     = PB,
                                       is_error         = EB,
                                       is_retransmitted = TB},

            Pkt#diameter_packet{header = Hdr,
                                bin = <<Vsn:8, Len:24,
                                        ?FLAGS(RB, PB, EB, TB), Code:24,
                                        Aid:32,
                                        Hid:32,
                                        Eid:32,
                                        Bin/binary>>}
    catch
        error: Reason: Stack ->
            Hdr = Hdr0#diameter_header{cmd_code = Code,
                                       application_id = Aid,
                                       is_request       = RB,
                                       is_proxiable     = PB,
                                       is_error         = EB,
                                       is_retransmitted = TB},
            exit({Reason, diameter_lib:stacktrace(Stack), Hdr})
    end.

%% values/1

values([H|T])
  when is_atom(H) ->
    T;
values(Avps) ->
    Avps.

%% encode_avps/4

%% Specifying values as a #diameter_avp list bypasses arity and other
%% checks: the values are expected to be already encoded and the AVP's
%% presented are simply sent. This is needed for relay agents, since
%% these have to be able to resend whatever comes.

%% Message as a list of #diameter_avp{} ...
encode_avps(_, _, [#diameter_avp{} | _] = Avps, Opts) ->
    encode_avps(Avps, Opts);

%% ... or as a tuple list or record.
encode_avps(Mod, MsgName, Values, Opts) ->
    Mod:encode_avps(MsgName, Values, Opts).

%% encode_avps/2

encode_avps(Avps, Opts) ->
    [pack_avp(A, Opts) || A <- Avps].

%% msg_header/3

msg_header(Mod, 'answer-message' = MsgName, Header) ->
    0 = Mod:id(),  %% assert
    #diameter_header{application_id = Aid,
                     cmd_code = Code}
        = Header,
    {-1, Flags, ?APP_ID_COMMON} = Mod:msg_header(MsgName),
    {Code, Flags, Aid};

msg_header(Mod, MsgName, _) ->
    Mod:msg_header(MsgName).

%% rec2msg/2

rec2msg(_, [Name|_])
  when is_atom(Name) ->
    Name;

rec2msg(Mod, Rec) ->
    Mod:rec2msg(element(1, Rec)).

%%% ---------------------------------------------------------------------------
%%% # decode/2
%%% ---------------------------------------------------------------------------

%% The representative default decode documented in diameter_codec(3).
%% As of the options that affect decode (eg. string_decode), it's no
%% longer *the* decode.

decode(Mod, Pkt) ->
    Opts = #{decode_format => record,
             string_decode => true,
             strict_mbit => true,
             rfc => 6733},
    decode(Mod, Opts, Pkt).

%%% ---------------------------------------------------------------------------
%%% # decode/3
%%% ---------------------------------------------------------------------------

%% Unsuccessfully decoded AVPs will be placed in #diameter_packet.errors.

-spec decode(module() | {module(), module()},
             map(),
             #diameter_packet{} | binary())
   -> #diameter_packet{}.

%% An Answer setting the E-bit. The application dictionary is needed
%% for the best-effort decode of Failed-AVP.
decode({Mod, AppMod}, Opts, Pkt) ->
    decode(Mod, AppMod, Opts, Pkt);

%% Or not: a request, or an answer not setting the E-bit.
decode(Mod, Opts, Pkt) ->
    decode(Mod, Mod, Opts, Pkt).

%% decode/4

decode(Id, Mod, Opts, Pkt)
  when is_integer(Id) ->
    decode(Id, Mod, Mod, Opts, Pkt);

decode(Mod, AppMod, Opts, Pkt) ->
    decode(Mod:id(), Mod, AppMod, Opts, Pkt).

%% decode/5

%% Relay application: just extract the avp's without any decoding of
%% their data since we don't know the application in question.
decode(?APP_ID_RELAY, _, _, _, #diameter_packet{} = Pkt) ->
    collect_avps(Pkt);

%% Otherwise decode using the dictionary.
decode(_, Mod, AppMod, Opts, #diameter_packet{header = Hdr} = Pkt) ->
    #diameter_header{cmd_code = CmdCode,
                     is_request = IsRequest,
                     is_error = IsError}
        = Hdr,

    MsgName = if IsError, not IsRequest ->
                      'answer-message';
                 true ->
                      Mod:msg_name(CmdCode, IsRequest)
              end,

    decode_avps(MsgName, Mod, AppMod, Opts, Pkt);

decode(Id, Mod, AppMod, Opts, Bin)
  when is_binary(Bin) ->
    decode(Id, Mod, AppMod, Opts, #diameter_packet{header = decode_header(Bin),
                                                   bin = Bin}).

%% decode_avps/5

decode_avps('', _, _, _, #diameter_packet{header = H,  %% unknown message
                                          bin = Bin}
                         = Pkt) ->
    ?LOG(unknown_message, H),
    Pkt#diameter_packet{avps = collect_avps(Bin),
                        errors = [3001]}; %% DIAMETER_COMMAND_UNSUPPORTED
%% msg = undefined identifies this case.

decode_avps(MsgName, Mod, AppMod, Opts, #diameter_packet{bin = Bin} = Pkt) ->
    {_, Avps} = split_binary(Bin, 20),
    {Rec, As, Errors} = Mod:decode_avps(MsgName,
                                        Avps,
                                        Opts#{app_dictionary => AppMod,
                                              failed_avp => false}),
    ?LOGC([] /= Errors, decode_errors, Pkt#diameter_packet.header),
    Pkt#diameter_packet{msg = reformat(MsgName, Rec, Opts),
                        errors = Errors,
                        avps = As}.

%% reformat/3

reformat(MsgName, Avps, #{decode_format := T})
  when T == map;
       T == list ->
    [MsgName | Avps];

reformat(_, Msg, _) ->
    Msg.

%%% ---------------------------------------------------------------------------
%%% # decode_header/1
%%% ---------------------------------------------------------------------------

-spec decode_header(binary())
   -> #diameter_header{}
    | false.

decode_header(<<Version:8,
                MsgLength:24,
                R:1, P:1, E:1, T:1, _:4,
                CmdCode:24,
                ApplicationId:32,
                HopByHopId:32,
                EndToEndId:32,
                _/binary>>) ->
    %% 3588 (ch 3) says that reserved bits MUST be set to 0 and ignored
    %% by the receiver.

    %% The RFC is quite unclear about the order of the bits in this
    %% case. It writes
    %%
    %%    0 1 2 3 4 5 6 7
    %%   +-+-+-+-+-+-+-+-+
    %%   |R P E T r r r r|
    %%   +-+-+-+-+-+-+-+-+
    %%
    %% in defining these but the scale refers to the (big endian)
    %% transmission order, first to last, not the bit order. That is,
    %% R is the high order bit. It's odd that a standard reserves
    %% low-order bit rather than high-order ones.

    #diameter_header{version = Version,
                     length = MsgLength,
                     cmd_code = CmdCode,
                     application_id = ApplicationId,
                     hop_by_hop_id = HopByHopId,
                     end_to_end_id = EndToEndId,
                     is_request       = 1 == R,
                     is_proxiable     = 1 == P,
                     is_error         = 1 == E,
                     is_retransmitted = 1 == T};

decode_header(_) ->
    false.

%%% ---------------------------------------------------------------------------
%%% # sequence_numbers/1
%%% ---------------------------------------------------------------------------

%% The End-To-End identifier must be unique for at least 4 minutes. We
%% maintain a 24-bit wraparound counter, and add an 8-bit persistent
%% wraparound counter. The 8-bit counter is incremented each time the
%% system is restarted.

-spec sequence_numbers(#diameter_packet{}
                       | #diameter_header{}
                       | binary()
                       | Seq)
   -> Seq
 when Seq :: {HopByHopId :: u32(), EndToEndId :: u32()}.

sequence_numbers({_,_} = T) ->
    T;

sequence_numbers(#diameter_packet{bin = Bin})
  when is_binary(Bin) ->
    sequence_numbers(Bin);

sequence_numbers(#diameter_packet{header = #diameter_header{} = H}) ->
    sequence_numbers(H);

sequence_numbers(#diameter_packet{msg = [#diameter_header{} = H | _]}) ->
    sequence_numbers(H);

sequence_numbers(#diameter_header{hop_by_hop_id = H,
                                  end_to_end_id = E}) ->
    {H,E};

sequence_numbers(<<_:12/binary, H:32, E:32, _/binary>>) ->
    {H,E}.

%%% ---------------------------------------------------------------------------
%%% # hop_by_hop_id/2
%%% ---------------------------------------------------------------------------

-spec hop_by_hop_id(u32(), binary())
   -> binary().

hop_by_hop_id(Id, <<H:12/binary, _:32, T/binary>>) ->
    <<H/binary, Id:32, T/binary>>.

%%% ---------------------------------------------------------------------------
%%% # msg_name/2
%%% ---------------------------------------------------------------------------

-spec msg_name(module(), #diameter_header{})
   -> atom()
    | {ApplicationId :: u32(), CommandCode :: u24(), Rbit :: u1()}.

msg_name(Dict0, #diameter_header{application_id = ?APP_ID_COMMON,
                                 cmd_code = C,
                                 is_request = R}) ->
    Dict0:msg_name(C,R);

msg_name(_, Hdr) ->
    msg_id(Hdr).

%% Note that messages in different applications could have the same
%% name.

%%% ---------------------------------------------------------------------------
%%% # msg_id/1
%%% ---------------------------------------------------------------------------

-spec msg_id(#diameter_packet{} | #diameter_header{})
   -> {ApplicationId :: u32(), CommandCode :: u24(), Rbit :: u1()}.

msg_id(#diameter_packet{msg = [#diameter_header{} = Hdr | _]}) ->
    msg_id(Hdr);

msg_id(#diameter_packet{header = #diameter_header{} = Hdr}) ->
    msg_id(Hdr);

msg_id(#diameter_header{application_id = A,
                        cmd_code = C,
                        is_request = R}) ->
    {A, C, ?BIT(R)};

msg_id(<<_:32, Rbit:1, _:7, CmdCode:24, ApplId:32, _/binary>>) ->
    {ApplId, CmdCode, Rbit}.

%%% ---------------------------------------------------------------------------
%%% # collect_avps/1
%%% ---------------------------------------------------------------------------

%% This is only used for the relay decode. Note that grouped avp's
%% aren't unraveled, only those at the top level.

-spec collect_avps(#diameter_packet{})
   -> #diameter_packet{};
                  (binary())
   -> [#diameter_avp{}].

collect_avps(#diameter_packet{bin = Bin} = Pkt) ->
    Pkt#diameter_packet{avps = collect_avps(Bin)};

collect_avps(<<_:20/binary, Avps/binary>>) ->
    collect(Avps).

%% collect/1

%%     0                   1                   2                   3
%%     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |                           AVP Code                            |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |V M P r r r r r|                  AVP Length                   |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |                        Vendor-ID (opt)                        |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |    Data ...
%%    +-+-+-+-+-+-+-+-+

collect(<<Code:32, V:1, M:1, P:1, _:5, Len:24, I:V/unit:32, Rest/binary>>) ->
    collect(Rest,
            Code,
            if 1 == V -> I; 0 == V -> undefined end,
            Len - 8 - V*4,  %% Might be negative, which ensures
            ?PAD(Len),      %%   failure of the match below.
            1 == M,
            1 == P);

collect(<<>>) ->
    [];

%% Header is truncated. pack_avp/1 will pad this at encode if sent in
%% a Failed-AVP.
collect(Bin) ->
    [#diameter_avp{data = {5014, Bin}}].

%% collect/7

collect(Bin, Code, Vid, DataLen, Pad, M, P) ->
    case Bin of
        <<Data:DataLen/binary, _:Pad/binary, Rest/binary>> ->
            Avp = #diameter_avp{code = Code,
                                vendor_id = Vid,
                                is_mandatory = M,
                                need_encryption = P,
                                data = Data},
            [Avp | collect(Rest)];
        _ ->
            %% Length in header points past the end of the message, or
            %% doesn't span the header. Note that an length error can
            %% only occur in the trailing AVP of a message or Grouped
            %% AVP, since a faulty AVP Length is otherwise
            %% indistinguishable from a correct one here, as we don't
            %% know the types of the AVPs being extracted.
            [#diameter_avp{code = Code,
                           vendor_id = Vid,
                           is_mandatory = M,
                           need_encryption = P,
                           data = {5014, Bin}}]
    end.

%% 3588:
%%
%%   DIAMETER_INVALID_AVP_LENGTH        5014
%%      The request contained an AVP with an invalid length.  A Diameter
%%      message indicating this error MUST include the offending AVPs
%%      within a Failed-AVP AVP.

%% 6733:
%%
%%    DIAMETER_INVALID_AVP_LENGTH 5014
%%
%%       The request contained an AVP with an invalid length.  A Diameter
%%       message indicating this error MUST include the offending AVPs
%%       within a Failed-AVP AVP.  In cases where the erroneous AVP length
%%       value exceeds the message length or is less than the minimum AVP
%%                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
%%       header length, it is sufficient to include the offending AVP
%%       ^^^^^^^^^^^^^
%%       header and a zero filled payload of the minimum required length
%%       for the payloads data type.  If the AVP is a Grouped AVP, the
%%       Grouped AVP header with an empty payload would be sufficient to
%%       indicate the offending AVP.  In the case where the offending AVP
%%       header cannot be fully decoded when the AVP length is less than
%%       the minimum AVP header length, it is sufficient to include an
%%       offending AVP header that is formulated by padding the incomplete
%%       AVP header with zero up to the minimum AVP header length.
%%
%% The underlined clause must be in error since (1) a header less than
%% the minimum value mean we might not know the identity of the AVP and
%% (2) the last sentence covers this case.

%%% ---------------------------------------------------------------------------
%%% # pack_avp/2
%%% ---------------------------------------------------------------------------

%% The normal case here is data as an #diameter_avp{} list or an
%% iolist, which are the cases that generated codec modules use. The
%% other cases are a convenience in the relay case in which the
%% dictionary doesn't know about specific AVP's.

%% Decoded Grouped AVP with decoded components: ignore components
%% since they're already encoded in the Grouped AVP.
pack_avp([#diameter_avp{} = Grouped | _Components], Opts) ->
    pack_avp(Grouped, Opts);

%% Grouped AVP whose components need packing. It's intentional that
%% this isn't equivalent to [Grouped | Components]: here the
%% components need to be encoded before wrapping with the Grouped AVP,
%% and the list is flat, nesting being accomplished in the data
%% fields.
pack_avp(#diameter_avp{data = [#diameter_avp{} | _] = Components}
         = Grouped,
         Opts) ->
    pack_data(Grouped, encode_avps(Components, Opts));

%% Data as a type/value tuple ...
pack_avp(#diameter_avp{data = {Type, Value}} = A, Opts)
  when is_atom(Type) ->
    pack_data(A, diameter_types:Type(encode, Value, Opts));

%% ... with a header in various forms ...
pack_avp(#diameter_avp{data = {T, {Type, Value}}}, Opts) ->
    pack_data(T, diameter_types:Type(encode, Value, Opts));

pack_avp(#diameter_avp{data = {T, Data}}, _) ->
    pack_data(T, Data);

pack_avp(#diameter_avp{data = {Dict, Name, Value}}, Opts) ->
    pack_data(Dict:avp_header(Name), Dict:avp(encode, Value, Name, Opts));

%% ... with a truncated header ...
pack_avp(#diameter_avp{code = undefined, data = B}, _)
  when is_binary(B) ->
    %% Reset the AVP Length of an AVP Header resulting from a 5014
    %% error. The RFC doesn't explicitly say to do this but the
    %% receiver can't correctly extract this and following AVP's
    %% without a correct length. On the downside, the header doesn't
    %% reveal if the received header has been padded. Discard bytes
    %% from the length header for this reason, to avoid creating a sub
    %% binary for no useful reason.
    Len = header_length(B),
    Sz = min(5, size(B)),
    <<B:Sz/binary, 0:(5-Sz)/unit:8, Len:24, 0:(Len-8)/unit:8>>;

%% Ignoring errors in Failed-AVP or during a relay encode.
pack_avp(#diameter_avp{data = {5014, Data}} = A, _) ->
    pack_data(A, Data);

pack_avp(#diameter_avp{data = Data} = A, _) ->
    pack_data(A, Data).

header_length(<<_:32, 1:1, _/bits>>) ->
    12;
header_length(_) ->
    8.

%%% ---------------------------------------------------------------------------
%%% # pack_data/2
%%% ---------------------------------------------------------------------------

pack_data(#diameter_avp{code = Code,
                        vendor_id = V,
                        is_mandatory = M,
                        need_encryption = P},
          Data) ->
    Flags = ?BIT(V /= undefined, 2#10000000)
        bor ?BIT(M, 2#01000000)
        bor ?BIT(P, 2#00100000),
    pack(Code, Flags, V, Data);

pack_data({Code, Flags, VendorId}, Data) ->
    pack(Code, Flags, VendorId, Data).

%% pack/4

pack(Code, Flags, VendorId, Data) ->
    Sz = iolist_size(Data),
    pack(Code, Flags, Sz, VendorId, Data, ?PAD(Sz)).
%% Padding is not included in the length field, as mandated by the RFC.

%% pack/6
%%
%% Prepend the vendor id as required.

pack(Code, Flags, Sz, _Vid, Data, Pad)
  when 0 == Flags band 2#10000000 ->
    pack(Code, Flags, Sz, 0, 0, Data, Pad);

pack(Code, Flags, Sz, Vid, Data, Pad) ->
    pack(Code, Flags, Sz+4, Vid, 1, Data, Pad).

%% pack/7

pack(Code, Flags, Sz, VId, V, Data, Pad) ->
    [<<Code:32, Flags:8, (8+Sz):24, VId:V/unit:32>>, Data, <<0:Pad/unit:8>>].
