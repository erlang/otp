%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
%% RFC 2671: Extension Mechanisms for DNS (EDNS0)
%% RFC 2782: A DNS RR for specifying the location of services (DNS SRV)
%% RFC 2915: The Naming Authority Pointer (NAPTR) DNS Resource Rec

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



-define(DECODE_ERROR, fmt). % must match a clause in inet_res:query_nss_e?dns

%%
%% Decode a dns buffer.
%%

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
	case Rest of
	    <<>> ->
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
		case QdTC or AnTC or NsTC or ArTC of
		    true when not HdrTC ->
			throw(?DECODE_ERROR);
		    _ ->
			#dns_rec{header=DnsHdr,
				 qdlist=QdList,
				 anlist=AnList,
				 nslist=NsList,
				 arlist=ArList}
		end;
	    _ ->
		%% Garbage data after DNS message
		throw(?DECODE_ERROR)
	end;
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
    case decode_name(Bin, Buffer) of
	{<<Type:16,Class:16,Rest/binary>>,Name} ->
	    DnsQuery =
		#dns_query{domain=Name,
			   type=decode_type(Type),
			   class=decode_class(Class)},
	    decode_query_section(Rest, N-1, Buffer, [DnsQuery|Qs]);
	_ ->
	    %% Broken question
	    throw(?DECODE_ERROR)
    end.

decode_rr_section(Bin, N, Buffer) ->
    decode_rr_section(Bin, N, Buffer, []).

decode_rr_section(<<>>=Rest, N, _Buffer, RRs) ->
    {Rest,reverse(RRs),N =/= 0};
decode_rr_section(Rest, 0, _Buffer, RRs) ->
    {Rest,reverse(RRs),false};
decode_rr_section(Bin, N, Buffer, RRs) ->
    case decode_name(Bin, Buffer) of
	{<<T:16/unsigned,C:16/unsigned,TTL:4/binary,
	  Len:16,D:Len/binary,Rest/binary>>,
	 Name} ->
	    Type = decode_type(T),
	    Class = decode_class(C),
	    Data = decode_data(D, Class, Type, Buffer),
	    RR =
		case Type of
		    opt ->
			<<ExtRcode,Version,Z:16>> = TTL,
			#dns_rr_opt{domain=Name,
				    type=Type,
				    udp_payload_size=C,
				    ext_rcode=ExtRcode,
				    version=Version,
				    z=Z,
				    data=Data};
		    _ ->
			<<TimeToLive:32/signed>> = TTL,
			#dns_rr{domain=Name,
				type=Type,
				class=Class,
				ttl=if TimeToLive < 0 -> 0;
				       true -> TimeToLive end,
				data=Data}
		end,
	    decode_rr_section(Rest, N-1, Buffer, [RR|RRs]);
	_ ->
	    %% Broken RR
	    throw(?DECODE_ERROR)
    end.

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
    Type = encode_type(Q#dns_query.type),
    Class = encode_class(Q#dns_query.class),
    {Bin,Comp} = encode_name(Bin0, Comp0, byte_size(Bin0), DName),
    encode_query_section(<<Bin/binary,Type:16,Class:16>>, Comp, Qs).

%% RFC 1035: 4.1.3. Resource record format
%% RFC 2671: 4.3, 4.4, 4.6 OPT RR format
%%
encode_res_section(Bin, Comp, []) -> {Bin,Comp};
encode_res_section(Bin, Comp, [#dns_rr {domain = DName,
					type = Type,
					class = Class,
					ttl = TTL,
					data = Data} | Rs]) ->
    encode_res_section_rr(Bin, Comp, Rs,
			  DName, Type, Class, <<TTL:32/signed>>, Data);
encode_res_section(Bin, Comp, [#dns_rr_opt {domain = DName,
					    udp_payload_size = UdpPayloadSize,
					    ext_rcode = ExtRCode,
					    version = Version,
					    z = Z,
					    data = Data} | Rs]) ->
    encode_res_section_rr(Bin, Comp, Rs,
			  DName, ?S_OPT, UdpPayloadSize,
			  <<ExtRCode,Version,Z:16>>, Data).

encode_res_section_rr(Bin0, Comp0, Rs, DName, Type, Class, TTL, Data) ->
    T = encode_type(Type),
    C = encode_class(Class),
    {Bin,Comp1} = encode_name(Bin0, Comp0, byte_size(Bin0), DName),
    {DataBin,Comp} = encode_data(Comp1, byte_size(Bin)+2+2+byte_size(TTL)+2,
				 Type, Class, Data),
    DataSize = byte_size(DataBin),
    encode_res_section(<<Bin/binary,T:16,C:16,
			TTL/binary,DataSize:16,DataBin/binary>>, Comp, Rs).

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
	Type when is_integer(Type) -> Type    %% raw unknown type
    end.

%%
%% Resource clases
%%

decode_class(Class) ->
    case Class of
	?C_IN -> in;
	?C_CHAOS ->  chaos;
	?C_HS -> hs;
	?C_ANY -> any;
	_ -> Class    %% raw unknown class
    end.

encode_class(Class) ->
    case Class of
	in -> ?C_IN;
	chaos -> ?C_CHAOS;
	hs -> ?C_HS;
	any -> ?C_ANY;
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
decode_data(<<A,B,C,D>>, in, ?S_A,  _)   -> {A,B,C,D};
decode_data(<<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>>, in, ?S_AAAA, _) ->
    {A,B,C,D,E,F,G,H};
decode_data(Dom, _, ?S_NS, Buffer)    -> decode_domain(Dom, Buffer);
decode_data(Dom, _, ?S_MD, Buffer)    -> decode_domain(Dom, Buffer); 
decode_data(Dom, _, ?S_MF, Buffer)    -> decode_domain(Dom, Buffer); 
decode_data(Dom, _, ?S_CNAME, Buffer) -> decode_domain(Dom, Buffer);
decode_data(Data0, _, ?S_SOA, Buffer) ->
    {Data1,MName} = decode_name(Data0, Buffer),
    {Data,RName} = decode_name(Data1, Buffer),
    case Data of
	<<Serial:32,Refresh:32/signed,Retry:32/signed,
	 Expiry:32/signed,Minimum:32>> ->
	    {MName,RName,Serial,Refresh,Retry,Expiry,Minimum};
	_ ->
	    %% Broken SOA RR data
	    throw(?DECODE_ERROR)
    end;
decode_data(Dom, _, ?S_MB, Buffer)    -> decode_domain(Dom, Buffer); 
decode_data(Dom, _, ?S_MG, Buffer)    -> decode_domain(Dom, Buffer); 
decode_data(Dom, _, ?S_MR, Buffer)    -> decode_domain(Dom, Buffer); 
decode_data(Data, _, ?S_NULL, _) -> Data;
decode_data(<<A,B,C,D,Proto,BitMap/binary>>, in, ?S_WKS, _Buffer) -> 
    {{A,B,C,D},Proto,BitMap};
decode_data(Dom, _, ?S_PTR, Buffer)   -> decode_domain(Dom, Buffer);
decode_data(<<CpuLen,CPU:CpuLen/binary,
	     OsLen,OS:OsLen/binary>>, _, ?S_HINFO, _) ->
    {binary_to_list(CPU),binary_to_list(OS)};
decode_data(Data0, _, ?S_MINFO, Buffer) ->
    {Data1,RM} = decode_name(Data0, Buffer),
    {Data,EM} = decode_name(Data1, Buffer),
    case Data of
	<<>> -> {RM,EM};
	_ ->
	    %% Broken MINFO data
	    throw(?DECODE_ERROR)
    end;
decode_data(<<Prio:16,Dom/binary>>, _, ?S_MX, Buffer) ->
    {Prio,decode_domain(Dom, Buffer)};
decode_data(<<Prio:16,Weight:16,Port:16,Dom/binary>>, _, ?S_SRV, Buffer) ->
    {Prio,Weight,Port,decode_domain(Dom, Buffer)};
decode_data(<<Order:16,Preference:16,Data0/binary>>, _, ?S_NAPTR, Buffer) ->
    {Data1,Flags} = decode_string(Data0),
    {Data2,Services} = decode_string(Data1),
    {Data,Regexp} = decode_characters(Data2, utf8),
    Replacement = decode_domain(Data, Buffer),
    {Order,Preference,string:lowercase(Flags),string:lowercase(Services),
     Regexp,Replacement};
%% ?S_OPT falls through to default
decode_data(Data, _, ?S_TXT, _) ->
    decode_txt(Data);
decode_data(Data, _, ?S_SPF, _) ->
    decode_txt(Data);
%% sofar unknown or non standard
decode_data(Data, _, _, _) ->
    Data.

%% Array of strings
%%
decode_txt(<<>>) -> [];
decode_txt(Bin) ->
    {Rest,String} = decode_string(Bin),
    [String|decode_txt(Rest)].

decode_string(<<Len,Bin:Len/binary,Rest/binary>>) ->
    {Rest,binary_to_list(Bin)};
decode_string(_) ->
    %% Broken string
    throw(?DECODE_ERROR).

decode_characters(<<Len,Bin:Len/binary,Rest/binary>>, Encoding) ->
    {Rest,unicode:characters_to_list(Bin, Encoding)};
decode_characters(_, _) ->
    %% Broken encoded string
    throw(?DECODE_ERROR).

%% One domain name only, there must be nothing after
%%
decode_domain(Bin, Buffer) ->
    case decode_name(Bin, Buffer) of
	{<<>>,Name} -> Name;
	_ ->
	    %% Garbage after domain name
	    throw(?DECODE_ERROR)
    end.

%% Domain name -> {RestBin,Name}
%%
decode_name(Bin, Buffer) ->
    decode_name(Bin, Buffer, [], Bin, 0).

%% Tail advances with Rest until the first indirection is followed
%% then it stays put at that Rest.
decode_name(_, Buffer, _Labels, _Tail, Cnt) when Cnt > byte_size(Buffer) ->
    throw(?DECODE_ERROR); %% Insantiy bailout - this must be a decode loop
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
    case Buffer of
	<<_:Ptr/binary,Bin/binary>> ->
	    decode_name(Bin, Buffer, Labels,
			if Cnt =/= 0 -> Tail; true -> Rest end,
			Cnt+2); % size of indirection pointer
	_ ->
	    %% Indirection pointer outside buffer
	    throw(?DECODE_ERROR)
    end;
decode_name(_, _, _, _, _) -> throw(?DECODE_ERROR).

%% Reverse list of labels (binaries) -> domain name (string)
decode_name_labels([]) -> ".";
decode_name_labels(Labels) ->
    decode_name_labels(Labels, "").

decode_name_labels([Label], Name) ->
    decode_name_label(Label, Name);
decode_name_labels([Label|Labels], Name) ->
    decode_name_labels(Labels, "."++decode_name_label(Label, Name)).

decode_name_label(<<>>, _Name) ->
    %% Empty label is only allowed for the root domain, 
    %% and that is handled above.
    throw(?DECODE_ERROR);
decode_name_label(Label, Name) ->
    decode_name_label(Label, Name, byte_size(Label)).

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
encode_data(Comp, _, ?S_A, in, {A,B,C,D}) -> {<<A,B,C,D>>,Comp};
encode_data(Comp, _, ?S_AAAA, in, {A,B,C,D,E,F,G,H}) ->
    {<<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>>,Comp};
encode_data(Comp, Pos, ?S_NS, in, Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, Pos, ?S_MD, in, Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, Pos, ?S_MF, in, Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, Pos, ?S_CNAME, in, Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp0, Pos, ?S_SOA, in,
	    {MName,RName,Serial,Refresh,Retry,Expiry,Minimum}) ->
    {B1,Comp1} = encode_name(Comp0, Pos, MName),
    {B,Comp} = encode_name(B1, Comp1, Pos+byte_size(B1), RName),
    {<<B/binary,Serial:32,Refresh:32/signed,Retry:32/signed,
      Expiry:32/signed,Minimum:32>>,
     Comp};
encode_data(Comp, Pos, ?S_MB, in, Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, Pos, ?S_MG, in, Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, Pos, ?S_MR, in, Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, _, ?S_NULL, in, Data) ->
    {iolist_to_binary(Data),Comp};
encode_data(Comp, _, ?S_WKS, in, {{A,B,C,D},Proto,BitMap}) ->
    BitMapBin = iolist_to_binary(BitMap),
    {<<A,B,C,D,Proto,BitMapBin/binary>>,Comp};
encode_data(Comp, Pos, ?S_PTR, in, Domain) -> encode_name(Comp, Pos, Domain);
encode_data(Comp, _, ?S_HINFO, in, {CPU,OS}) ->
    Bin = encode_string(iolist_to_binary(CPU)),
    {encode_string(Bin, iolist_to_binary(OS)),Comp};
encode_data(Comp0, Pos, ?S_MINFO, in, {RM,EM}) ->
    {Bin,Comp} = encode_name(Comp0, Pos, RM),
    encode_name(Bin, Comp, Pos+byte_size(Bin), EM);
encode_data(Comp, Pos, ?S_MX, in, {Pref,Exch}) ->
    encode_name(<<Pref:16>>, Comp, Pos+2, Exch);
encode_data(Comp, Pos, ?S_SRV, in, {Prio,Weight,Port,Target}) ->
    encode_name(<<Prio:16,Weight:16,Port:16>>, Comp, Pos+2+2+2, Target);
encode_data(Comp, Pos, ?S_NAPTR, in, 
	    {Order,Preference,Flags,Services,Regexp,Replacement}) ->
    B0 = <<Order:16,Preference:16>>,
    B1 = encode_string(B0, iolist_to_binary(Flags)),
    B2 = encode_string(B1, iolist_to_binary(Services)),
    B3 = encode_string(B2, unicode:characters_to_binary(Regexp,
							unicode, utf8)),
    %% Bypass name compression (RFC 2915: section 2)
    {B,_} = encode_name(B3, gb_trees:empty(), Pos+byte_size(B3), Replacement),
    {B,Comp};
%% ?S_OPT falls through to default
encode_data(Comp, _, ?S_TXT, in, Data) -> {encode_txt(Data),Comp};
encode_data(Comp, _, ?S_SPF, in, Data) -> {encode_txt(Data),Comp};
encode_data(Comp, _Pos, _Type, _Class, Data) -> {iolist_to_binary(Data),Comp}.

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

name2labels("") -> [];
name2labels(".") -> [];
name2labels(Cs) -> name2labels(<<>>, Cs).
%%
-compile({inline, [name2labels/2]}).
name2labels(Label, "") -> [Label];
name2labels(Label, ".") -> [Label];
name2labels(Label, "."++Cs) -> [Label|name2labels(<<>>, Cs)];
name2labels(Label, "\\"++[C|Cs]) -> name2labels(<<Label/binary,C>>, Cs);
name2labels(Label, [C|Cs]) -> name2labels(<<Label/binary,C>>, Cs).

%% Fail on empty or too long labels.
encode_labels(Bin, Comp, _Pos, []) ->
    {<<Bin/binary,0>>,Comp};
encode_labels(Bin, Comp0, Pos, [L|Ls]=Labels)
  when 1 =< byte_size(L), byte_size(L) =< 63 ->
    case gb_trees:lookup(Labels, Comp0) of
	none ->
	    Comp = if Pos < (3 bsl 14) ->
			   %% Just in case - compression
			   %% pointers can not reach further
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
