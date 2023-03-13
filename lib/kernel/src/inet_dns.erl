%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2023. All Rights Reserved.
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
-module(inet_dns).

%% Dns record encode/decode
%%
%% RFC 1035: Domain Names - Implementation and Specification
%% RFC 2181: Clarifications to the DNS Specification
%% RFC 6891: Extension Mechanisms for DNS (EDNS0)
%% RFC 2782: A DNS RR for specifying the location of services (DNS SRV)
%% RFC 2915: The Naming Authority Pointer (NAPTR) DNS Resource Rec
%% RFC 6488: DNS Certification Authority Authorization (CAA) Resource Record
%% RFC 7553: The Uniform Resource Identifier (URI) DNS Resource Record
%% RFC 6762: Multicast DNS

-export([decode/1, encode/1]).

-import(lists, [reverse/1]).

-include("inet_int.hrl").
-include("inet_dns.hrl").

-export([record_type/1, rr/1, rr/2]).
-export([make_rr/0, make_rr/1, make_rr/2, make_rr/3]).
%% ADTs exports. The make_* functions are undocumented.
-export([msg/1, msg/2,
	 make_msg/0, make_msg/1, make_msg/2, make_msg/3]).
-export([header/1, header/2,
	 make_header/0, make_header/1, make_header/2, make_header/3]).
-export([dns_query/1, dns_query/2,
	 make_dns_query/0, make_dns_query/1,
	 make_dns_query/2, make_dns_query/3]).
-include("inet_dns_record_adts.hrl").


%% Function merge of #dns_rr{} and #dns_rr_opt{}
%%

record_type(#dns_rr{}) -> rr;
record_type(#dns_rr_opt{}) -> rr;
record_type(Rec) ->
    record_adts(Rec).

rr(#dns_rr{}=RR) -> dns_rr(RR);
rr(#dns_rr_opt{}=RR) -> dns_rr_opt(RR).

rr(#dns_rr{}=RR, L) -> dns_rr(RR, L);
rr(#dns_rr_opt{}=RR, L) -> dns_rr_opt(RR, L).

make_rr() -> make_dns_rr().

make_rr(L) when is_list(L) ->
    case rr_type(L, any) of
	opt -> make_dns_rr_opt(L);
	_ -> make_dns_rr(L)
    end.

make_rr(type, opt) -> make_dns_rr_opt();
make_rr(F, V) when is_atom(F) -> make_dns_rr(F, V);
make_rr(#dns_rr{}=RR, L) when is_list(L) ->
    case rr_type(L, RR#dns_rr.type) of
	opt ->
	    Ts = common_fields__rr__rr_opt(),
	    make_dns_rr_opt([Opt || {T,_}=Opt <- dns_rr(RR),
				    lists_member(T, Ts)] ++ L);
	_ -> make_dns_rr(RR, L)
    end;
make_rr(#dns_rr_opt{}=RR, L) when is_list(L) ->
    case rr_type(L, RR#dns_rr_opt.type) of
	opt ->
	     make_dns_rr_opt(RR, L);
	_ ->
	    Ts = common_fields__rr__rr_opt(),
	    make_dns_rr([Opt || {T,_}=Opt <- dns_rr_opt(RR),
				lists_member(T, Ts)] ++ L)
    end.

make_rr(#dns_rr{}=RR, type, opt) -> make_rr(RR, [{type,opt}]);
make_rr(#dns_rr{}=RR, F, V) -> make_dns_rr(RR, F, V);
make_rr(#dns_rr_opt{}=RR, type, opt) -> RR;
make_rr(#dns_rr_opt{}=RR, type, T) -> make_rr(RR, [{type,T}]);
make_rr(#dns_rr_opt{}=RR, F, V) -> make_dns_rr_opt(RR, F, V).

-compile({inline, [rr_type/2]}).
rr_type([], T) -> T;
rr_type([{type,T}|Opts], _) -> rr_type(Opts, T);
rr_type([_|Opts], T) -> rr_type(Opts, T).

common_fields__rr__rr_opt() ->
    [T || T <- record_info(fields, dns_rr_opt),
	  lists_member(T, record_info(fields, dns_rr))].

-compile({inline, [lists_member/2]}).
lists_member(_, []) -> false;
lists_member(H, [H|_]) -> true;
lists_member(H, [_|T]) -> lists_member(H, T).


-define(in_range(Low, X, High), ((Low =< (X)) andalso ((X) =< High))).
-define(is_decimal(X), (?in_range(0, (X), 9))).


%% must match a clause in inet_res:query_nss_e?dns
-define(DECODE_ERROR, formerr).

%%
%% Decode a dns buffer.
%%

%% Match macros that throw(?DECODE_ERROR) for no match
-define(
   MATCH_ELSE_DECODE_ERROR(Match, Pattern, Result),
   case begin Match end of
       (Pattern) ->
           begin Result end;
       _ ->
           throw(?DECODE_ERROR)
   end).
-define(
   MATCH_ELSE_DECODE_ERROR(Match, Pattern, Guard, Result),
   case begin Match end of
       (Pattern) when not not (Guard) ->
           begin Result end;
       _ ->
           throw(?DECODE_ERROR)
   end).

decode(Buffer) when is_binary(Buffer) ->
    try do_decode(Buffer) of
	DnsRec ->
	    {ok,DnsRec}
    catch
	Reason ->
	    {error,Reason}
    end.

do_decode(<<Id:16,
	   QR:1,Opcode:4,AA:1,TC:1,RD:1,
	   RA:1,PR:1,_:2,Rcode:4,
	   QdCount:16,AnCount:16,NsCount:16,ArCount:16,
	   QdBuf/binary>>=Buffer) ->
    {AnBuf,QdList,QdTC} = decode_query_section(QdBuf,QdCount,Buffer),
    {NsBuf,AnList,AnTC} = decode_rr_section(AnBuf,AnCount,Buffer),
    {ArBuf,NsList,NsTC} = decode_rr_section(NsBuf,NsCount,Buffer),
    {Rest,ArList,ArTC} = decode_rr_section(ArBuf,ArCount,Buffer),
    ?MATCH_ELSE_DECODE_ERROR(
       Rest,
       <<>>,
       begin
           HdrTC = decode_boolean(TC),
           DnsHdr =
               #dns_header{id=Id,
                           qr=decode_boolean(QR),
                           opcode=decode_opcode(Opcode),
                           aa=decode_boolean(AA),
                           tc=HdrTC,
                           rd=decode_boolean(RD),
                           ra=decode_boolean(RA),
                           pr=decode_boolean(PR),
                           rcode=Rcode},
           ?MATCH_ELSE_DECODE_ERROR(
              %% Header marked as truncated, or no section
              %% marked as truncated.
              %% The converse; a section marked as truncated,
              %% but not the header - is a parse error.
              %%
              HdrTC or (not (QdTC or AnTC or NsTC or ArTC)),
              true,
              begin
                  #dns_rec{header=DnsHdr,
                           qdlist=QdList,
                           anlist=AnList,
                           nslist=NsList,
                           arlist=ArList}
              end)
       end);
do_decode(_) ->
    %% DNS message does not even match header
    throw(?DECODE_ERROR).

decode_query_section(Bin, N, Buffer) ->
    decode_query_section(Bin, N, Buffer, []).

decode_query_section(<<>>=Rest, N, _Buffer, Qs) ->
    {Rest,reverse(Qs),N =/= 0};
decode_query_section(Rest, 0, _Buffer, Qs) ->
    {Rest,reverse(Qs),false};
decode_query_section(Bin, N, Buffer, Qs) ->
    ?MATCH_ELSE_DECODE_ERROR(
       decode_name(Bin, Buffer),
       {<<T:16,C:16,Rest/binary>>,Name},
       begin
           {Class,UnicastResponse} = decode_class(C),
           DnsQuery =
               #dns_query{
                  domain           = Name,
                  type             = decode_type(T),
                  class            = Class,
                  unicast_response = UnicastResponse},
           decode_query_section(Rest, N-1, Buffer, [DnsQuery|Qs])
       end).

decode_rr_section(Bin, N, Buffer) ->
    decode_rr_section(Bin, N, Buffer, []).

decode_rr_section(<<>>=Rest, N, _Buffer, RRs) ->
    {Rest,reverse(RRs),N =/= 0};
decode_rr_section(Rest, 0, _Buffer, RRs) ->
    {Rest,reverse(RRs),false};
decode_rr_section(Bin, N, Buffer, RRs) ->
    ?MATCH_ELSE_DECODE_ERROR(
       decode_name(Bin, Buffer),
       {<<T:16/unsigned,C:16/unsigned,TTL:4/binary,
	  Len:16,D:Len/binary,Rest/binary>>,
        Name},
       begin
           Type = decode_type(T),
           RR =
               case Type of
                   ?S_OPT ->
                       <<ExtRcode,Version,DO:1,Z:15>> = TTL,
                       DnssecOk = (DO =/= 0),
                       #dns_rr_opt{
                          domain           = Name,
                          type             = Type,
                          udp_payload_size = C,
                          ext_rcode        = ExtRcode,
                          version          = Version,
                          z                = Z,
                          data             = D,
                          do               = DnssecOk};
                   _ ->
                       {Class,CacheFlush} = decode_class(C),
                       Data = decode_data(D, Class, Type, Buffer),
                       <<TimeToLive:32/signed>> = TTL,
                       #dns_rr{
                          domain = Name,
                          type   = Type,
                          class  = Class,
                          ttl    = max(0, TimeToLive),
                          data   = Data,
                          func   = CacheFlush}
               end,
           decode_rr_section(Rest, N-1, Buffer, [RR|RRs])
       end).

%%
%% Encode a user query
%%

encode(Q) ->
    QdCount = length(Q#dns_rec.qdlist),
    AnCount = length(Q#dns_rec.anlist),
    NsCount = length(Q#dns_rec.nslist),
    ArCount = length(Q#dns_rec.arlist),
    B0 = encode_header(Q#dns_rec.header, QdCount, AnCount, NsCount, ArCount),
    C0 = gb_trees:empty(),
    {B1,C1} = encode_query_section(B0, C0, Q#dns_rec.qdlist),
    {B2,C2} = encode_res_section(B1, C1, Q#dns_rec.anlist),
    {B3,C3} = encode_res_section(B2, C2, Q#dns_rec.nslist),
    {B,_} = encode_res_section(B3, C3, Q#dns_rec.arlist),
    B.


%% RFC 1035: 4.1.1. Header section format
%%
encode_header(#dns_header{id=Id}=H, QdCount, AnCount, NsCount, ArCount) ->
    QR = encode_boolean(H#dns_header.qr),
    Opcode = encode_opcode(H#dns_header.opcode),
    AA = encode_boolean(H#dns_header.aa),
    TC = encode_boolean(H#dns_header.tc),
    RD = encode_boolean(H#dns_header.rd),
    RA = encode_boolean(H#dns_header.ra),
    PR = encode_boolean(H#dns_header.pr),
    Rcode = H#dns_header.rcode,
    <<Id:16,
     QR:1,Opcode:4,AA:1,TC:1,RD:1,
     RA:1,PR:1,0:2,Rcode:4,
     QdCount:16,AnCount:16,NsCount:16,ArCount:16>>.

%% RFC 1035: 4.1.2. Question section format
%%
encode_query_section(Bin, Comp, []) -> {Bin,Comp};
encode_query_section(Bin0, Comp0, [#dns_query{domain=DName}=Q | Qs]) ->
    T = encode_type(Q#dns_query.type),
    C = encode_class(Q#dns_query.class, Q#dns_query.unicast_response),
    {Bin,Comp} = encode_name(Bin0, Comp0, byte_size(Bin0), DName),
    encode_query_section(<<Bin/binary,T:16,C:16>>, Comp, Qs).

%% RFC 1035:  4.1.3.               Resource record format
%% RFC 6891:  6.1.2, 6.1.3, 6.2.3  Opt RR format
%%
encode_res_section(Bin, Comp, []) -> {Bin,Comp};
encode_res_section(
  Bin, Comp,
  [#dns_rr{
      domain = DName,
      type   = Type,
      class  = Class,
      func   = CacheFlush,
      ttl    = TTL,
      data   = Data} | Rs]) ->
    encode_res_section_rr(
      Bin, Comp, Rs, DName, Type, Class, CacheFlush,
      <<TTL:32/signed>>, Data);
encode_res_section(
  Bin, Comp,
  [#dns_rr_opt{
      domain           = DName,
      udp_payload_size = UdpPayloadSize,
      ext_rcode        = ExtRCode,
      version          = Version,
      z                = Z,
      data             = Data,
      do               = DnssecOk} | Rs]) ->
    DO = case DnssecOk of true -> 1; false -> 0 end,
    encode_res_section_rr(
      Bin, Comp, Rs, DName, ?S_OPT, UdpPayloadSize, false,
      <<ExtRCode,Version,DO:1,Z:15>>, Data).

encode_res_section_rr(
  Bin0, Comp0, Rs, DName, Type, Class, CacheFlush, TTL, Data) ->
    T = encode_type(Type),
    C = encode_class(Class, CacheFlush),
    {Bin,Comp1} = encode_name(Bin0, Comp0, byte_size(Bin0), DName),
    Pos = byte_size(Bin)+2+2+byte_size(TTL)+2,
    {DataBin,Comp} = encode_data(Comp1, Pos, Type, Class, Data),
    DataSize = byte_size(DataBin),
    encode_res_section(
      <<Bin/binary,T:16,C:16,TTL/binary,DataSize:16,DataBin/binary>>,
      Comp, Rs).

%%
%% Resource types
%%
decode_type(Type) ->
    case Type of
	?T_A -> ?S_A;
	?T_NS -> ?S_NS;
	?T_MD -> ?S_MD;
	?T_MF -> ?S_MF;
	?T_CNAME -> ?S_CNAME;
	?T_SOA -> ?S_SOA;
	?T_MB  -> ?S_MB;
	?T_MG  -> ?S_MG;
	?T_MR  -> ?S_MR;
	?T_NULL -> ?S_NULL;
	?T_WKS  -> ?S_WKS;
	?T_PTR  -> ?S_PTR;
	?T_HINFO -> ?S_HINFO;
	?T_MINFO -> ?S_MINFO;
	?T_MX -> ?S_MX;
	?T_TXT -> ?S_TXT;
	?T_AAAA -> ?S_AAAA;
	?T_LOC -> ?S_LOC;
	?T_SRV -> ?S_SRV;
	?T_NAPTR -> ?S_NAPTR;
	?T_OPT -> ?S_OPT;
	?T_SPF -> ?S_SPF;
	%% non standard
	?T_UINFO -> ?S_UINFO;
	?T_UID -> ?S_UID;
	?T_GID -> ?S_GID;
	?T_UNSPEC -> ?S_UNSPEC;
	%% Query type values which do not appear in resource records
	?T_AXFR -> ?S_AXFR;
	?T_MAILB -> ?S_MAILB;
	?T_MAILA -> ?S_MAILA;
	?T_ANY  -> ?S_ANY;
	?T_URI  -> ?S_URI;
	?T_CAA  -> ?S_CAA;
	_ -> Type    %% raw unknown type
    end.

%%
%% Resource types
%%
encode_type(Type) ->
    case Type of
	?S_A -> ?T_A;
	?S_NS -> ?T_NS;
	?S_MD -> ?T_MD;
	?S_MF -> ?T_MF;
	?S_CNAME -> ?T_CNAME;
	?S_SOA -> ?T_SOA;
	?S_MB -> ?T_MB;
	?S_MG -> ?T_MG;
	?S_MR -> ?T_MR;
	?S_NULL -> ?T_NULL;
	?S_WKS -> ?T_WKS;
	?S_PTR -> ?T_PTR;
	?S_HINFO -> ?T_HINFO;
	?S_MINFO -> ?T_MINFO;
	?S_MX -> ?T_MX;
	?S_TXT -> ?T_TXT;
	?S_AAAA -> ?T_AAAA;
	?S_LOC -> ?T_LOC;
	?S_SRV -> ?T_SRV;
	?S_NAPTR -> ?T_NAPTR;
	?S_OPT -> ?T_OPT;
	?S_SPF -> ?T_SPF;
	%% non standard
	?S_UINFO -> ?T_UINFO;
	?S_UID -> ?T_UID;
	?S_GID -> ?T_GID;
	?S_UNSPEC -> ?T_UNSPEC;
	%% Query type values which do not appear in resource records
	?S_AXFR -> ?T_AXFR;
	?S_MAILB -> ?T_MAILB;
	?S_MAILA -> ?T_MAILA;
	?S_ANY -> ?T_ANY;
	?S_URI -> ?T_URI;
	?S_CAA -> ?T_CAA;
	Type when is_integer(Type) -> Type    %% raw unknown type
    end.


%%
%% Resource classes
%%

decode_class(C0) ->
    FlagBit = 16#8000,
    C = C0 band (bnot FlagBit),
    Class =
        case C of
            ?C_IN    -> in;
            ?C_CHAOS -> chaos;
            ?C_HS    -> hs;
            ?C_ANY   -> any;
            _ -> C    %% raw unknown class
        end,
    Flag = (C0 band FlagBit) =/= 0,
    {Class,Flag}.


encode_class(Class, Flag) ->
    C = encode_class(Class),
    case Flag of
        true  -> FlagBit = 16#8000, C bor FlagBit;
        false -> C
    end.
%%
encode_class(Class) ->
    case Class of
	in    -> ?C_IN;
	chaos -> ?C_CHAOS;
	hs    -> ?C_HS;
	any   -> ?C_ANY;
	Class when is_integer(Class) -> Class    %% raw unknown class
    end.

decode_opcode(Opcode) ->
    case Opcode of
	?QUERY -> 'query';
	?IQUERY -> iquery;
	?STATUS -> status;
	_ when is_integer(Opcode) -> Opcode %% non-standard opcode
    end.

encode_opcode(Opcode) ->
    case Opcode of
	'query' -> ?QUERY;
	iquery -> ?IQUERY;
	status -> ?STATUS;
	_ when is_integer(Opcode) -> Opcode %% non-standard opcode
    end.
	    

encode_boolean(true) -> 1;
encode_boolean(false) -> 0;
encode_boolean(B) when is_integer(B) -> B.

decode_boolean(0) -> false;
decode_boolean(I) when is_integer(I) -> true.


%%
%% Data field -> term() content representation
%%
%% Class IN RRs
decode_data(Data, in, ?S_A,  _) ->
    ?MATCH_ELSE_DECODE_ERROR(Data, <<A,B,C,D>>, {A,B,C,D});
decode_data(Data, in, ?S_AAAA, _) ->
    ?MATCH_ELSE_DECODE_ERROR(
       Data,
       <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>>,
       {A,B,C,D,E,F,G,H});
decode_data(Data, in, ?S_WKS, _) ->
    ?MATCH_ELSE_DECODE_ERROR(
       Data,
       <<A,B,C,D,Proto,BitMap/binary>>,
       {{A,B,C,D},Proto,BitMap});
%%
decode_data(Data, Class, Type, Buffer) ->
    if
        is_integer(Class) -> % Raw class
            Data;
        is_atom(Class) -> % Symbolic class, i.e: known
            decode_data(Data, Type, Buffer)
    end.
%%
%%
%% Standard RRs (any class)
decode_data(Data, ?S_SOA, Buffer) ->
    {Data1,MName} = decode_name(Data, Buffer),
    {Data2,RName} = decode_name(Data1, Buffer),
    ?MATCH_ELSE_DECODE_ERROR(
       Data2,
       <<Serial:32,Refresh:32/signed,Retry:32/signed,
         Expiry:32/signed,Minimum:32>>,
       {MName,RName,Serial,Refresh,Retry,Expiry,Minimum});
decode_data(Data, ?S_NS,    Buffer) -> decode_domain(Data, Buffer);
decode_data(Data, ?S_MD,    Buffer) -> decode_domain(Data, Buffer);
decode_data(Data, ?S_MF,    Buffer) -> decode_domain(Data, Buffer);
decode_data(Data, ?S_CNAME, Buffer) -> decode_domain(Data, Buffer);
decode_data(Data, ?S_MB,    Buffer) -> decode_domain(Data, Buffer);
decode_data(Data, ?S_MG,    Buffer) -> decode_domain(Data, Buffer);
decode_data(Data, ?S_MR,    Buffer) -> decode_domain(Data, Buffer);
decode_data(Data, ?S_PTR,   Buffer) -> decode_domain(Data, Buffer);
decode_data(Data, ?S_NULL,  _)      -> Data;
decode_data(Data, ?S_HINFO, _) ->
    ?MATCH_ELSE_DECODE_ERROR(
       Data,
       <<CpuLen,CPU:CpuLen/binary,OsLen,OS:OsLen/binary>>,
       {binary_to_list(CPU),binary_to_list(OS)});
decode_data(Data, ?S_MINFO, Buffer) ->
    {Data1,RM} = decode_name(Data, Buffer),
    {Data2,EM} = decode_name(Data1, Buffer),
    ?MATCH_ELSE_DECODE_ERROR(Data2, <<>>, {RM,EM});
decode_data(Data, ?S_MX, Buffer) ->
    ?MATCH_ELSE_DECODE_ERROR(
       Data,
       <<Prio:16,Dom/binary>>,
       {Prio,decode_domain(Dom, Buffer)});
decode_data(Data, ?S_LOC, _) ->
    ?MATCH_ELSE_DECODE_ERROR(
       Data,
       <<Version:8, SizeBase:4, SizeExp:4,
         HorizPreBase:4, HorizPreExp:4, VertPreBase:4, VertPreExp:4,
         Latitude:32, Longitude:32, Altitude:32>>,
       ((Version =:= 0) andalso
        ?is_decimal(SizeBase) andalso ?is_decimal(SizeExp) andalso
        ?is_decimal(HorizPreBase) andalso ?is_decimal(HorizPreExp) andalso
        ?is_decimal(VertPreBase) andalso ?is_decimal(VertPreExp)),
       {{decode_loc_angle(Latitude), decode_loc_angle(Longitude)},
        decode_loc_altitude(Altitude),
        decode_loc_size(SizeBase, SizeExp),
        {decode_loc_size(HorizPreBase, HorizPreExp),
         decode_loc_size(VertPreBase, VertPreExp)}});
decode_data(Data, ?S_SRV, Buffer) ->
    ?MATCH_ELSE_DECODE_ERROR(
       Data,
       <<Prio:16,Weight:16,Port:16,Dom/binary>>,
       {Prio,Weight,Port,decode_domain(Dom, Buffer)});
decode_data(Data, ?S_NAPTR, Buffer) ->
    ?MATCH_ELSE_DECODE_ERROR(
       Data,
       <<Order:16,Preference:16,Data1/binary>>,
       begin
           {Data2,Flags} = decode_string(Data1),
           {Data3,Services} = decode_string(Data2),
           {Data4,Regexp} = decode_characters(Data3, utf8),
           Replacement = decode_domain(Data4, Buffer),
           {Order,Preference,
            inet_db:tolower(Flags),inet_db:tolower(Services),
            Regexp,Replacement}
       end);
decode_data(Data, ?S_TXT, _) -> decode_txt(Data);
decode_data(Data, ?S_SPF, _) -> decode_txt(Data);
decode_data(Data, ?S_URI, _) ->
    ?MATCH_ELSE_DECODE_ERROR(
       Data,
       <<Prio:16,Weight:16,Data1/binary>>, 1 =< byte_size(Data1),
       begin
           Target = binary_to_list(Data1),
           {Prio,Weight,Target}
       end);
decode_data(Data, ?S_CAA, _) ->
    ?MATCH_ELSE_DECODE_ERROR(
       Data,
       <<Flags:8,Data1/binary>>,
       begin
           {Data2,Tag} = decode_string(Data1),
           ?MATCH_ELSE_DECODE_ERROR(
              length(Tag),
              L, 1 =< L andalso L =< 15,
              begin
                  Value = binary_to_list(Data2),
                  {Flags,inet_db:tolower(Tag),Value}
              end)
       end);
%%
%% sofar unknown or non standard
decode_data(Data, Type, _) when is_integer(Type) ->
    Data.


%% Array of strings
%%
decode_txt(<<>>) -> [];
decode_txt(Bin) ->
    {Rest,String} = decode_string(Bin),
    [String|decode_txt(Rest)].

decode_string(Data) ->
    ?MATCH_ELSE_DECODE_ERROR(
       Data,
       <<Len,Bin:Len/binary,Rest/binary>>,
       {Rest,binary_to_list(Bin)}).

decode_characters(Data, Encoding) ->
    ?MATCH_ELSE_DECODE_ERROR(
       Data,
       <<Len,Bin:Len/binary,Rest/binary>>,
       {Rest,unicode:characters_to_list(Bin, Encoding)}).

%% One domain name only, there must be nothing after
%%
decode_domain(Bin, Buffer) ->
    ?MATCH_ELSE_DECODE_ERROR(decode_name(Bin, Buffer), {<<>>,Name}, Name).

%% Domain name -> {RestBin,Name}
%%
decode_name(Bin, Buffer) ->
    decode_name(Bin, Buffer, [], Bin, 0).

%% Tail advances with Rest until the first indirection is followed
%% then it stays put at that Rest.
decode_name(_, Buffer, _Labels, _Tail, Cnt) when Cnt > byte_size(Buffer) ->
    throw(?DECODE_ERROR); %% Insanity bailout - this must be a decode loop
decode_name(<<0,Rest/binary>>, _Buffer, Labels, Tail, Cnt) ->
    %% Root domain, we have all labels for the domain name
    {if Cnt =/= 0 -> Tail; true -> Rest end,
     decode_name_labels(Labels)};
decode_name(<<0:2,Len:6,Label:Len/binary,Rest/binary>>,
	     Buffer, Labels, Tail, Cnt) ->
    %% One plain label here
    decode_name(Rest, Buffer, [Label|Labels],
		if Cnt =/= 0 -> Tail; true -> Rest end,
		Cnt);
decode_name(<<3:2,Ptr:14,Rest/binary>>, Buffer, Labels, Tail, Cnt) ->
    %% Indirection - reposition in buffer and recurse
    ?MATCH_ELSE_DECODE_ERROR(
       Buffer,
       <<_:Ptr/binary,Bin/binary>>,
       decode_name(
         Bin, Buffer, Labels,
         if Cnt =/= 0 -> Tail; true -> Rest end,
         Cnt+2)); % size of indirection pointer
decode_name(_, _, _, _, _) -> throw(?DECODE_ERROR).

%% Reverse list of labels (binaries) -> domain name (string)
decode_name_labels([]) -> ".";
decode_name_labels(Labels) ->
    decode_name_labels(Labels, "").

decode_name_labels([Label], Name) ->
    decode_name_label(Label, Name);
decode_name_labels([Label|Labels], Name) ->
    decode_name_labels(Labels, "."++decode_name_label(Label, Name)).

decode_name_label(Label, Name) ->
    ?MATCH_ELSE_DECODE_ERROR(
       Label,
       _, 1 =< byte_size(Label),
       %% Empty label is only allowed for the root domain,
       %% and that is handled above.
       decode_name_label(Label, Name, byte_size(Label))).

%% Decode $. and $\\ to become $\\ escaped characters
%% in the string representation.
-compile({inline, [decode_name_label/3]}).
decode_name_label(_, Name, 0) -> Name;
decode_name_label(Label, Name, N) ->
    M = N-1,
    case Label of
	<<_:M/binary,($\\),_/binary>> ->
	    decode_name_label(Label, "\\\\"++Name, M);
	<<_:M/binary,($.),_/binary>> ->
	    decode_name_label(Label, "\\."++Name, M);
	<<_:M/binary,C,_/binary>> ->
	    decode_name_label(Label, [C|Name], M);
	_ ->
	    %% This should not happen but makes surrounding
	    %% programming errors easier to locate.
	    erlang:error(badarg, [Label,Name,N])
    end.

%%
%% Data field -> {binary(),NewCompressionTable}
%%
%% Class IN RRs
encode_data(Comp, _, ?S_A, in, Addr) ->
    {A,B,C,D} = Addr,
    {<<A,B,C,D>>,Comp};
encode_data(Comp, _, ?S_AAAA, in, Addr) ->
    {A,B,C,D,E,F,G,H} = Addr,
    {<<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>>,Comp};
encode_data(Comp, _, ?S_WKS, in, Data) ->
    {{A,B,C,D},Proto,BitMap} = Data,
    BitMapBin = iolist_to_binary(BitMap),
    {<<A,B,C,D,Proto,BitMapBin/binary>>,Comp};
%% OPT pseudo-RR (of no class) - should not take this way;
%% this must be a #dns_rr{type = ?S_OPT} instead of a #dns_rr_opt{},
%% so good luck getting in particular Class and TTL right...
encode_data(Comp, _, ?S_OPT, _UdpPayloadSize, Data) ->
    encode_data(Comp, Data);
%%
encode_data(Comp, Pos, Type, Class, Data) ->
    if
        is_integer(Class) ->
            encode_data(Comp, Data);
        is_atom(Class) -> % Known Class
            encode_data(Comp, Pos, Type, Data)
    end.
%%
%%
%% Standard RRs (any class)
encode_data(Comp, Pos, ?S_SOA, Data) ->
    {MName,RName,Serial,Refresh,Retry,Expiry,Minimum} = Data,
    {B1,Comp1} = encode_name(Comp, Pos, MName),
    {B,Comp2} = encode_name(B1, Comp1, Pos+byte_size(B1), RName),
    {<<B/binary,Serial:32,Refresh:32/signed,Retry:32/signed,
      Expiry:32/signed,Minimum:32>>,
     Comp2};
encode_data(Comp, Pos, ?S_NS,    Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, Pos, ?S_MD,    Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, Pos, ?S_MF,    Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, Pos, ?S_CNAME, Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, Pos, ?S_MB,    Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, Pos, ?S_MG,    Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, Pos, ?S_MR,    Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, Pos, ?S_PTR,   Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, _,   ?S_NULL,  Data)   -> encode_data(Comp, Data);
encode_data(Comp, _,   ?S_HINFO, Data) ->
    {CPU,OS} = Data,
    Bin = encode_string(iolist_to_binary(CPU)),
    {encode_string(Bin, iolist_to_binary(OS)),Comp};
encode_data(Comp, Pos, ?S_MINFO, Data) ->
    {RM,EM} = Data,
    {Bin,Comp1} = encode_name(Comp, Pos, RM),
    encode_name(Bin, Comp1, Pos+byte_size(Bin), EM);
encode_data(Comp, Pos, ?S_MX, Data) ->
    {Pref,Exch} = Data,
    encode_name(<<Pref:16>>, Comp, Pos+2, Exch);
encode_data(Comp, _, ?S_LOC, Data) ->
    %% Similar to the Master File Format in section 3 of RFC 1876
    case Data of
        {{Latitude, Longitude}, Altitude, Size, {HorizPre, VertPre}} ->
            ok;
        {{Latitude, Longitude}, Altitude, Size} ->
            HorizPre = 10_000_00, VertPre = 10_00,
            ok;
        {{Latitude, Longitude}, Altitude} ->
            Size = 1_00, HorizPre = 10_000_00, VertPre = 10_00,
            ok
    end,
    Version = 0,
    {<<Version:8, (encode_loc_size(Size))/binary,
       (encode_loc_size(HorizPre))/binary, (encode_loc_size(VertPre))/binary,
       (encode_loc_angle(Latitude)):32,
       (encode_loc_angle(Longitude)):32,
       (encode_loc_altitude(Altitude)):32>>,
     Comp};
encode_data(Comp, Pos, ?S_SRV, Data) ->
    {Prio,Weight,Port,Target} = Data,
    encode_name(<<Prio:16,Weight:16,Port:16>>, Comp, Pos+2+2+2, Target);
encode_data(Comp, Pos, ?S_NAPTR, Data) ->
    {Order,Preference,Flags,Services,Regexp,Replacement} = Data,
    B0 = <<Order:16,Preference:16>>,
    B1 = encode_string(B0, iolist_to_binary(Flags)),
    B2 = encode_string(B1, iolist_to_binary(Services)),
    B3 = encode_string(B2, unicode:characters_to_binary(Regexp,
							unicode, utf8)),
    %% Bypass name compression (RFC 2915: section 2)
    {B,_} = encode_name(B3, gb_trees:empty(), Pos+byte_size(B3), Replacement),
    {B,Comp};
encode_data(Comp, _, ?S_TXT, Data) -> {encode_txt(Data),Comp};
encode_data(Comp, _, ?S_SPF, Data) -> {encode_txt(Data),Comp};
encode_data(Comp, _, ?S_URI, Data) ->
    {Prio,Weight,Target} = Data,
    {<<Prio:16,Weight:16,(iolist_to_binary(Target))/binary>>,Comp};
encode_data(Comp, _, ?S_CAA, Data)->
    case Data of
        {Flags,Tag,Value} ->
            B0 = <<Flags:8>>,
            B1 = encode_string(B0, iolist_to_binary(Tag)),
            B2 = iolist_to_binary(Value),
            {<<B1/binary,B2/binary>>,Comp};
        _ ->
            {encode_txt(Data),Comp}
    end;
%%
%% sofar unknown or non standard
encode_data(Comp, _Pos, Type, Data) when is_integer(Type) ->
    encode_data(Comp, Data).
%%
%% Passthrough
-compile({inline, [encode_data/2]}).
encode_data(Comp, Data) ->
    {iolist_to_binary(Data),Comp}.

%% Array of strings
%%
encode_txt(Strings) ->
    encode_txt(<<>>, Strings).
%%
encode_txt(Bin, []) -> Bin;
encode_txt(Bin, [S|Ss]) ->
    encode_txt(encode_string(Bin, iolist_to_binary(S)), Ss).

%% Singular string
%%
encode_string(StringBin) ->
    encode_string(<<>>, StringBin).
%%
encode_string(Bin, StringBin) ->
    Size = byte_size(StringBin),
    if Size =< 255 ->
	    <<Bin/binary,Size,StringBin/binary>>
    end.

%% Domain name
%%
encode_name(Comp, Pos, Name) ->
    encode_name(<<>>, Comp, Pos, Name).
%%
%% Bin = target binary
%% Comp = compression lookup table; label list -> buffer position
%% Pos = position in DNS message
%% Name = domain name to encode
%%
%% The name compression does not make the case conversions
%% it could. This means case will be preserved at the cost
%% of missed compression opportunities. But if the encoded
%% message use the same case for different instances of
%% the same domain name there is no problem, and if not it is
%% only compression that suffers. Furthermore encode+decode
%% this way becomes an identity operation for any decoded
%% DNS message which is nice for testing encode.
%% 
encode_name(Bin0, Comp0, Pos, Name) ->
    case encode_labels(Bin0, Comp0, Pos, name2labels(Name)) of
	{Bin,_}=Result when byte_size(Bin) - byte_size(Bin0) =< 255 -> Result;
	_ ->
	    %% Fail on too long name
	    erlang:error(badarg, [Bin0,Comp0,Pos,Name])
    end.

name2labels("") ->  [];
name2labels(".") -> [];
name2labels(Cs) ->  name2labels(<<>>, Cs).
%%
name2labels(Label, "") ->            [Label];
name2labels(Label, ".") ->           [Label];
name2labels(Label, "."++Cs) ->       [Label|name2labels(<<>>, Cs)];
name2labels(Label, "\\"++[C|Cs]) ->  name2labels(Label, Cs, C);
name2labels(Label, [C|Cs]) ->        name2labels(Label, Cs, C).
%%
-compile({inline, [name2labels/3]}).
name2labels(Label, Cs, C) when is_integer(C), 0 =< C, C =< 255 ->
    name2labels(<<Label/binary,C>>, Cs).

%% Fail on empty or too long labels.
encode_labels(Bin, Comp, _Pos, []) ->
    {<<Bin/binary,0>>,Comp};
encode_labels(Bin, Comp0, Pos, [L|Ls]=Labels)
  when 1 =< byte_size(L), byte_size(L) =< 63 ->
    case gb_trees:lookup(Labels, Comp0) of
	none ->
	    Comp = if Pos < (1 bsl 14) ->
			   %% Just in case - compression
			   %% pointers cannot reach further
			   gb_trees:insert(Labels, Pos, Comp0);
		      true -> Comp0
		   end,
	    Size = byte_size(L),
	    encode_labels(<<Bin/binary,Size,L/binary>>,
			  Comp, Pos+1+Size, Ls);
	{value,Ptr} ->
	    %% Name compression - point to already encoded name
	    {<<Bin/binary,3:2,Ptr:14>>,Comp0}
    end.


decode_loc_angle(X) ->
    (X - 16#8000_0000) / 3600_000.

encode_loc_angle(X) when is_float(X) ->
    %% Degrees (1/360 of a turn)
    encode_loc_angle(round(X * 3600_000));
encode_loc_angle(X)
  when is_integer(X), -16#8000_0000 =< X, X =< 16#7FFF_FFFF ->
    %% 1/1000:s of arc second
    X + 16#8000_0000.  % Zero is encoded as 2^31


decode_loc_altitude(X) ->
    (X - 100_000_00) / 100.

encode_loc_altitude(X) when is_float(X) ->
    %% Meters
    encode_loc_altitude(round(X * 100));
encode_loc_altitude(X)
  when is_integer(X), -100_000_00 =< X, X =< 16#FFFF_FFFF - 100_000_00 ->
    %% Centimeters above a base level 100_000 m below
    %% the GPS reference spheroid [DoD WGS-1984]
    X + 100_000_00.


decode_loc_size(Base, Exponent) ->
    round(Base * math:pow(10, Exponent)) / 100.

%% Return the smallest encoded value >= X;
%% a bit like ceil(X) of encoded values
%%
encode_loc_size(X) when is_float(X) ->
    %% Meters
    encode_loc_size(round(X * 100));
encode_loc_size(0) ->
    0;
encode_loc_size(X)
  when is_integer(X), 0 =< X, X =< 9000_000_000 ->
    %% Centimeters, to be encoded as Digit * 10^Exponent
    %% with both Digit and Exponent in 0..9,
    %% limiting the range to 0..9e9
    %%
    Exponent = floor(math:log10((X - 0.05) / 0.9)),
    Multiplier = round(math:pow(10, Exponent)),
    Base = (X + Multiplier - 1) div Multiplier,
    <<Base:4, Exponent:4>>.
