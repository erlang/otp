%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: RFC 4566
%%----------------------------------------------------------------------

-module(megaco_sdp).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include_lib("megaco/include/megaco_sdp.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([
	 decode/1, encode/1, 
	 get_sdp_record_from_PropertyGroup/2
        ]).


%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------


%%======================================================================
%% External functions
%%======================================================================

%% ---------------------------------------------------------------------
%% decode(PP) -> {ok, SDP} | {error, Reason}
%% 
%% This function performs the following conversion:
%% property_parm()   -> sdp()
%% property_group()  -> sdp_property_group()
%% property_groups() -> sdp_property_groups()
%% 
%% ---------------------------------------------------------------------

decode(SDP) ->
    case (catch do_decode(SDP)) of
	{ok, _} = OK ->
	    OK;
	{error, _} = ERR ->
	    ERR;
	{'EXIT', Reason} ->
	    {error, {exit, Reason}};
	CRAP ->
	    {error, {crap, CRAP}}
    end.

do_decode(PP) when is_record(PP, 'PropertyParm') ->
    decode_PropertyParm(PP);
do_decode([PP|_] = PG) when is_record(PP, 'PropertyParm') ->
    decode_PropertyGroup(PG);
do_decode([H|_] = PGs) when is_list(H) ->
    decode_PropertyGroups(PGs);
do_decode(asn1_NOVALUE = V) ->
    {ok, V};
do_decode(Bad) ->
    {error, {bad_sdp, Bad}}.


%% ---------------------------------------------------------------------
%% encode(SDPs) -> {ok, PP} | {error, Reason}
%% 
%% This function performs the following conversion:
%% sdp()                 -> property_parm()
%% sdp_property_group()  -> property_group()
%% sdp_property_groups() -> property_groups()
%% 
%% ---------------------------------------------------------------------

encode(SDP) ->
    case (catch do_encode(SDP)) of
	{ok, _} = OK ->
	    OK;
	{error, _} = ERR ->
	    ERR;
	{'EXIT', Reason} ->
	    {error, {exit, Reason}};
	CRAP ->
	    {error, {crap, CRAP}}
    end.

do_encode(SDP) when is_tuple(SDP) ->
    {ok, encode_PropertyParm(SDP)};
do_encode([SDP|_] = PG) when is_tuple(SDP) ->
    encode_PropertyGroup(PG);
do_encode([H|_] = PGs) when is_list(H) ->
    encode_PropertyGroups(PGs);
do_encode(asn1_NOVALUE = V) ->
    {ok, V}.


%%-----------------------------------------------------------------
%% Generate sdp records from PropertyParm records
%%-----------------------------------------------------------------

decode_PropertyGroups(PGs) ->
    decode_PropertyGroups(PGs, []).

decode_PropertyGroups([], DecodedPGs) ->
    {ok, lists:reverse(DecodedPGs)};

decode_PropertyGroups([PG | PGs], DecodedPGs) ->
    {ok, DecodedPG} = decode_PropertyGroup(PG),
    decode_PropertyGroups(PGs, [DecodedPG | DecodedPGs]).


decode_PropertyGroup(PG) ->
    {ok, decode_PropertyGroup(PG, [])}.

decode_PropertyGroup([], Acc) ->
    lists:reverse(Acc);

decode_PropertyGroup([PP | PG], Acc) ->
    case (catch decode_PropertyParm(PP)) of
	{ok, PP2} ->
	    decode_PropertyGroup(PG, [PP2 | Acc]);
	{error, Reason} ->
	    decode_PropertyGroup(PG, [{PP, Reason} | Acc]);
	Error ->
	    decode_PropertyGroup(PG, [{PP, Error} | Acc])
    end.


%% ===== Protocol Version =====
%% 
decode_PropertyParm(#'PropertyParm'{name      = "v", 
				    value     = [V],
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_protocol_version(V);


%% ===== Origin =====
%% 
decode_PropertyParm(#'PropertyParm'{name      = "o", 
				    value     = [V], 
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_origin(V);


%% ===== Session Name =====
%% 
decode_PropertyParm(#'PropertyParm'{name      = "s", 
				    value     = [V], 
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_session_name(V);


%% ===== Session and Media Information =====
%% 
decode_PropertyParm(#'PropertyParm'{name      = "i", 
				    value     = [V], 
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_session_media_id(V);


%% ===== URI =====
%% 
decode_PropertyParm(#'PropertyParm'{name      = "u", 
				    value     = [V], 
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_uri(V);


%% ===== Email Address and Phone Number =====
%% 
decode_PropertyParm(#'PropertyParm'{name      = "e", 
				    value     = [V], 
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_email(V);

decode_PropertyParm(#'PropertyParm'{name      = "p", 
				    value     = [V], 
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_phone(V);


%% ===== Connection Data =====
%% 
decode_PropertyParm(#'PropertyParm'{name      = "c", 
				    value     = [V],
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_connection_data(V);


%% ===== Bandwidth =====
%% 
decode_PropertyParm(#'PropertyParm'{name      = "b", 
				    value     = [V], 
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_bandwidth(V);


%% ===== Times, Repeat Times and Time Zones =====
%% 
decode_PropertyParm(#'PropertyParm'{name      = "t", 
				    value     = [V], 
				    extraInfo = asn1_NOVALUE }) ->
    decode_pp_times(V);
decode_PropertyParm(#'PropertyParm'{name      = "r", 
				    value     = [V], 
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_rtimes(V);
decode_PropertyParm(#'PropertyParm'{name      = "z", 
				    value     = [V], 
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_tzones(V);


%% ===== Encryption Keys =====
%% 
decode_PropertyParm(#'PropertyParm'{name      = "k", 
				    value     = [V], 
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_encryption_keys(V);


%% ===== Attributes =====
%% 
decode_PropertyParm(#'PropertyParm'{name      = "a", 
				    value     = [V], 
				    extraInfo =  asn1_NOVALUE}) ->
    decode_pp_attribute(V);


%% ===== Media Announcements =====
%% 
decode_PropertyParm(#'PropertyParm'{name      = "m", 
				    value     = [V], 
				    extraInfo = asn1_NOVALUE}) ->
    decode_pp_media_announcement(V);


decode_PropertyParm(_PP) ->
    ?d("decode_PropertyParm -> entry with"
       "~n   _PP: ~p", [_PP]),
    {error, undefined_PropertyParm}.


%%-----------------------------------------------------------------
%% Generate PropertyParm records from sdp records
%%-----------------------------------------------------------------

encode_PropertyGroups(PGs) ->
    encode_PropertyGroups(PGs, []).


encode_PropertyGroups([], Acc) ->
    {ok, lists:reverse(Acc)};
encode_PropertyGroups([PG | PGs], Acc) ->
    {ok, EncodedPG} = encode_PropertyGroup(PG), 
    encode_PropertyGroups(PGs, [EncodedPG | Acc]).


encode_PropertyGroup(PG) ->
    encode_PropertyGroup(PG, []).

encode_PropertyGroup([], Acc) ->
    {ok, lists:reverse(Acc)};
encode_PropertyGroup([PP | PG], Acc) ->
    EncodedPP = encode_PropertyParm(PP),
    encode_PropertyGroup(PG, [EncodedPP | Acc]).
    

%% ===== Protocol Version =====
%% 
encode_PropertyParm(#megaco_sdp_v{version = Version}) ->
    encode_pp_protocol_version(Version);


%% ===== Origin =====
%% 
encode_PropertyParm(#megaco_sdp_o{user_name    = User,
				  session_id   = SessionId,
				  version      = Version,
				  network_type = Network,
				  address_type = AddrType,
				  address      = Addr}) ->
    encode_pp_origin(User, SessionId, Version, Network, AddrType, Addr);    


%% ===== Session Name =====
%% 
encode_PropertyParm(#megaco_sdp_s{name = Name}) ->
    encode_pp_session_name(Name);    


%% ===== Session and Media Information =====
%% 
encode_PropertyParm(#megaco_sdp_i{session_descriptor = SD}) ->
    encode_pp_session_media_id(SD);


%% ===== URI =====
%% 
encode_PropertyParm(#megaco_sdp_u{uri = URI}) ->
    encode_pp_uri(URI);


%% ===== Email Address and Phone Number =====
%% 
encode_PropertyParm(#megaco_sdp_e{email = Email}) ->
    encode_pp_email(Email);

encode_PropertyParm(#megaco_sdp_p{phone_number = Num}) ->
    encode_pp_phone(Num);


%% ===== Connection Data =====
%% 
encode_PropertyParm(#megaco_sdp_c{network_type    = NetType,
				  address_type    = AddressType,
				  connection_addr = ConnectionAddr}) ->
    encode_pp_connection_data(NetType, AddressType, ConnectionAddr);


%% ===== Bandwidth =====
%% 
encode_PropertyParm(#megaco_sdp_b{bwtype    = BwType,
				  bandwidth = Bandwidth}) ->
    encode_pp_bandwidth(BwType, Bandwidth);


%% ===== Times, Repeat Times and Time Zones =====
%% 
encode_PropertyParm(#megaco_sdp_t{start = Start, stop = Stop}) ->
    encode_pp_times(Start, Stop);

encode_PropertyParm(#megaco_sdp_r{repeat_interval = Repeat,
				  active_duration = Duration, 
				  list_of_offsets = ListOfOffsets}) ->
    encode_pp_rtimes(Repeat, Duration, ListOfOffsets);

encode_PropertyParm(#megaco_sdp_z{list_of_adjustments = LOA}) ->
    encode_pp_tzones(LOA);


%% ===== Encryption Keys =====
%% 
encode_PropertyParm(#megaco_sdp_k{method         = Method, 
				  encryption_key = EncryptionKey}) ->
    encode_pp_encryption_keys(Method, EncryptionKey);


%% ===== Attributes =====
%% 
encode_PropertyParm(#megaco_sdp_a_cat{category = Category}) ->
    encode_pp_attribute_cat(Category);

encode_PropertyParm(#megaco_sdp_a_keywds{keywords = Keywords}) ->
    encode_pp_attribute_keywds(Keywords);

encode_PropertyParm(#megaco_sdp_a_tool{name_and_version = NameAndVersion}) ->
    encode_pp_attribute_tool(NameAndVersion);

encode_PropertyParm(#megaco_sdp_a_ptime{packet_time = PacketTime}) ->
    encode_pp_attribute_ptime(PacketTime);

encode_PropertyParm(
  #megaco_sdp_a_maxptime{maximum_packet_time = MaxPacketTime}) ->
    encode_pp_attribute_maxptime(MaxPacketTime);

encode_PropertyParm(#megaco_sdp_a_rtpmap{payload_type   = Payload, 
					 encoding_name  = EncName,
					 clock_rate     = ClockRate, 
					 encoding_parms = EncPar}) ->
    encode_pp_attribute_rtpmap(Payload, EncName, ClockRate, EncPar);

encode_PropertyParm(#megaco_sdp_a_orient{orientation = Orientation}) ->
    encode_pp_attribute_orient(Orientation);

encode_PropertyParm(#megaco_sdp_a_type{conf_type = CType}) ->
    encode_pp_attribute_type(CType);

encode_PropertyParm(#megaco_sdp_a_charset{char_set = CharSet}) ->
    encode_pp_attribute_charset(CharSet);

encode_PropertyParm(#megaco_sdp_a_sdplang{tag = Tag}) ->
    encode_pp_attribute_sdplang(Tag);

encode_PropertyParm(#megaco_sdp_a_lang{tag = Tag}) ->
    encode_pp_attribute_lang(Tag);

encode_PropertyParm(#megaco_sdp_a_framerate{frame_rate = FrameRate}) ->
    encode_pp_attribute_framerate(FrameRate);

encode_PropertyParm(#megaco_sdp_a_quality{quality = Quality}) ->
    encode_pp_attribute_quality(Quality);

encode_PropertyParm(#megaco_sdp_a_fmtp{format = Fmt, param = Params}) ->
    encode_pp_attribute_fmtp(Fmt, Params);

encode_PropertyParm(#megaco_sdp_a{attribute = Attr, value = Value}) ->
    encode_pp_attribute(Attr, Value);


%% ===== Media Announcements =====
%% 
encode_PropertyParm(#megaco_sdp_m{media     = Media,
				  port      = Port,
				  num_ports = NOP,
				  transport = Transport,
				  fmt_list  = FMT}) ->
    encode_pp_media_announcement(Media, Port, NOP, Transport, FMT);


%% This is a "manually" encoded PropertyParm, leave it as is.
%% 
encode_PropertyParm(PP) when is_record(PP, 'PropertyParm') ->
    PP;


%% Bad data
encode_PropertyParm(SDP) ->
    error({unknown_sdp, SDP}).


%%-----------------------------------------------------------------
%% Func: get_sdp_record_from_PropertGroup/2
%% Description: Get all sdp records of a certain type from a 
%%              property group
%%-----------------------------------------------------------------

get_sdp_record_from_PropertyGroup(Type, PG) 
  when is_atom(Type) and is_list(PG) ->
    F = fun(R) -> not is_pg_record(Type, R) end,
    lists:filter(F, PG).

is_pg_record(v, R) when is_record(R, megaco_sdp_v)        -> true;
is_pg_record(c, R) when is_record(R, megaco_sdp_c)        -> true;
is_pg_record(m, R) when is_record(R, megaco_sdp_m)        -> true;
is_pg_record(o, R) when is_record(R, megaco_sdp_o)        -> true;
is_pg_record(a, R) when is_record(R, megaco_sdp_a)        -> true;
is_pg_record(a, R) when is_record(R, megaco_sdp_a_ptime)  -> true;
is_pg_record(a, R) when is_record(R, megaco_sdp_a_rtpmap) -> true;
is_pg_record(b, R) when is_record(R, megaco_sdp_b)        -> true;
is_pg_record(t, R) when is_record(R, megaco_sdp_t)        -> true;
is_pg_record(r, R) when is_record(R, megaco_sdp_r)        -> true;
is_pg_record(z, R) when is_record(R, megaco_sdp_z)        -> true;
is_pg_record(k, R) when is_record(R, megaco_sdp_k)        -> true;
is_pg_record(s, R) when is_record(R, megaco_sdp_s)        -> true;
is_pg_record(i, R) when is_record(R, megaco_sdp_i)        -> true;
is_pg_record(u, R) when is_record(R, megaco_sdp_u)        -> true;
is_pg_record(e, R) when is_record(R, megaco_sdp_e)        -> true;
is_pg_record(p, R) when is_record(R, megaco_sdp_p)        -> true;
is_pg_record(_, _)                                        -> false.

    
%%======================================================================
%% Internal functions
%%======================================================================

%% ===== Protocol Version =====
%% 
decode_pp_protocol_version(Value) when is_list(Value) ->
    ?d("decode_pp_protocol_version -> entry with"
       "~n   Value: ~p", [Value]),
    Version = s2i(Value, invalid_protocol_version),
    ?d("decode_pp_protocol_version -> entry with"
       "~n   Version: ~w", [Version]),
    decode_pp_protocol_version(Version);
decode_pp_protocol_version(Version) when is_integer(Version) ->
    ?d("decode_pp_protocol_version -> entry with"
       "~n   Version: ~w", [Version]),
    {ok, #megaco_sdp_v{version = Version}}.

encode_pp_protocol_version(Version) when is_integer(Version) ->
    ?d("encode_pp_protocol_version -> entry with"
       "~n   Version: ~w", [Version]),
    #'PropertyParm'{name  =  "v", 
		    value = [integer_to_list(Version)]};
encode_pp_protocol_version(Version) ->
    error({invalid_protocol_version, Version}).


%% ===== Origin =====
%% 
decode_pp_origin(Value) ->
    ?d("decode_pp_origin -> entry with"
       "~n   Value: ~p", [Value]),
    case string:tokens(Value, " \t") of
	[User, SessId, SessVersion, NetType, AddrType, UnicastAddress] ->
	    ?d("decode_pp_origin -> entry with"
	       "~n   User:           ~p"
	       "~n   SessionId:      ~p"
	       "~n   Version:        ~p"
	       "~n   NetType:        ~p"
	       "~n   AddrType:       ~p"
	       "~n   UnicastAddress: ~p", 
	       [User, SessId, SessVersion, NetType, AddrType, UnicastAddress]),
	    F = fun(X, R) -> 
			case (catch list_to_integer(X)) of
			    I when is_integer(I) ->
				I;
			    _ ->
				error({invalid_origin, {R, X}})
			end
		end,
	    SID = F(SessId,      sess_id), 
	    V   = F(SessVersion, sess_version), 
	    SDP = #megaco_sdp_o{user_name    = User,
				session_id   = SID,
				version      = V,
				network_type = decode_network_type(NetType),
				address_type = decode_address_type(AddrType),
				address      = UnicastAddress},
	    {ok, SDP};
	Err ->
	    ?d("decode_pp_origin -> "
	       "~n   Err: ~p", [Err]),
	    invalid_pp(origin, Value, Err)
    end.
    
encode_pp_origin(User0, SessionId0, SessVersion0, 
		 NetType0, AddrType0, UnicastAddr0) ->
    ?d("do_encode_pp_origin -> entry with"
       "~n   User0:        ~p"
       "~n   SessionId0:   ~p"
       "~n   SessVersion0: ~p"
       "~n   NetType0:     ~p"
       "~n   AddrType0:    ~p"
       "~n   UnicastAddr0: ~p", 
       [User0, SessionId0, SessVersion0, NetType0, AddrType0, UnicastAddr0]),
    User        = encode_origin_user(User0),
    SessionId   = encode_origin_session_id(SessionId0),
    SessVersion = encode_origin_sess_version(SessVersion0),
    NetType     = encode_origin_network_type(NetType0), 
    AddrType    = encode_origin_address_type(AddrType0), 
    UnicastAddr = encode_origin_unicast_address(UnicastAddr0), 
    #'PropertyParm'{name  = "o", 
		    value = [
			     User        ++ " " ++
			     SessionId   ++ " " ++
			     SessVersion ++ " " ++
			     NetType     ++ " " ++
			     AddrType    ++ " " ++
			     UnicastAddr
			    ]}.

encode_origin_user(User) when is_list(User) ->
    User;
encode_origin_user(BadUser) ->
    error({invalid_origin_user, BadUser}).

encode_origin_session_id(SID) when is_integer(SID) ->
    integer_to_list(SID);
encode_origin_session_id(BadSID) ->
    error({invalid_origin_session_id, BadSID}).

encode_origin_sess_version(SessVersion) when is_integer(SessVersion) ->
    integer_to_list(SessVersion);
encode_origin_sess_version(BadSessVersion) ->
    error({invalid_origin_sess_version, BadSessVersion}).

encode_origin_network_type(NT) ->
    case (catch encode_network_type(NT)) of
	{error, _} ->
	    error({invalid_origin_network_type, NT});
	Val ->
	    Val
    end.

encode_origin_address_type(AT) ->
    case (catch encode_address_type(AT)) of
	{error, _} ->
	    error({invalid_origin_address_type, AT});
	Val ->
	    Val
    end.

encode_origin_unicast_address(UnicastAddr) when is_list(UnicastAddr) ->
    UnicastAddr;
encode_origin_unicast_address(BadUnicastAddr) ->
    error({invalid_origin_unicast_address, BadUnicastAddr}).


%% ===== Session Name =====
%% 
decode_pp_session_name(Value) ->
    ?d("decode_pp_session_name -> entry with"
       "~n   Value: ~p", [Value]),
    {ok, #megaco_sdp_s{name = Value}}.

encode_pp_session_name(Name) when is_list(Name) ->
    ?d("encode_pp_session_name -> entry with"
       "~n   Name: '~s'", [Name]),
    #'PropertyParm'{name  = "s", 
		    value = [Name]};
encode_pp_session_name(BadName) ->
    error({invalid_session_name, BadName}).

    
%% ===== Session and Media Information =====
%% 
decode_pp_session_media_id(Value) ->
    ?d("decode_pp_session_media_id -> entry with"
       "~n   Value: ~p", [Value]),
    {ok, #megaco_sdp_i{session_descriptor = Value}}.

encode_pp_session_media_id(SD) when is_list(SD) ->
    ?d("encode_pp_session_media_id -> entry with"
       "~n   SD: '~s'", [SD]),
    #'PropertyParm'{name  = "i", 
		    value = [SD]};
encode_pp_session_media_id(BadSD) ->
    error({invalid_session_media_id, BadSD}).


%% ===== URI =====
%% 
decode_pp_uri(Value) ->
    ?d("decode_pp_uri -> entry with"
       "~n   Value: ~p", [Value]),
    {ok, #megaco_sdp_u{uri = Value}}.

encode_pp_uri(URI) when is_list(URI) ->
    ?d("encode_pp_uri -> entry with"
       "~n   URI: ~p", [URI]),
    #'PropertyParm'{name  = "u", 
		    value = [URI]};
encode_pp_uri(BadUri) ->
    error({invalid_uri, BadUri}).


%% ===== Email Address and Phone Number =====
%% 
decode_pp_email(Value) ->
    ?d("decode_pp_email -> entry with"
       "~n   Value: ~p", [Value]),
    {ok, #megaco_sdp_e{email = Value}}.

encode_pp_email(Email) when is_list(Email) ->
    ?d("encode_pp_email -> entry with"
       "~n   Email: ~p", [Email]),
    #'PropertyParm'{name  = "e", 
		    value = [Email]};
encode_pp_email(BadEmail) ->
    error({invalid_email, BadEmail}).
   
decode_pp_phone(Value) ->
    ?d("decode_pp_phone -> entry with"
       "~n   Value: ~p", [Value]),
    {ok, #megaco_sdp_p{phone_number = Value}}.

encode_pp_phone(Phone) when is_list(Phone) ->
    ?d("encode_pp_phone -> entry with"
       "~n   Phone: ~p", [Phone]),
    #'PropertyParm'{name  = "p", 
		    value = [Phone]};
encode_pp_phone(BadPhone) ->
    error({invalid_phone, BadPhone}).


%% ===== Connection Data =====
%% 
decode_pp_connection_data(Value) ->
    ?d("decode_pp_connection_data -> entry with"
       "~n   Value: ~p", [Value]),
    case string:tokens(Value, " \t") of
	[NetType, AddrType, ConnectionAddr] ->
	    ?d("decode_pp_connection_data -> "
	       "~n   NetType:        ~p"
	       "~n   AddrType:       ~p"
	       "~n   ConnectionAddr: ~p", [NetType, AddrType, ConnectionAddr]),
	    NT = decode_network_type(NetType), 
	    AT = decode_address_type(AddrType), 
	    case AT of
		ip4 ->
		    ?d("decode_pp_connection_data -> ip4", []),
		    ConnAddr = 
			case string:tokens(ConnectionAddr, "\/") of
			    [Base, TtlStr, NumOfAddrs] ->
				?d("decode_pp_connection_data -> "
				   "~n   Base:      ~p"
				   "~n   TtlStr:    ~p"
				   "~n   NumOfAddrs:~p", 
				   [Base, TtlStr, NumOfAddrs]),
				TTL = s2i(TtlStr, 
					  invalid_connection_data_ttl),
				?d("decode_pp_connection_data -> TTL: ~p", 
				   [TTL]),
				NOA = 
				    s2i(NumOfAddrs, 
					invalid_connection_data_conn_addr_num_of),
				?d("decode_pp_connection_data -> NOA: ~p", 
				   [NOA]),
				#megaco_sdp_c_conn_addr{base   = Base,
							ttl    = TTL,
							num_of = NOA};
			    [Base, TtlStr] ->
				?d("decode_pp_connection_data -> "
				   "~n   Base:   ~p"
				   "~n   TtlStr: ~p", 
				   [Base, TtlStr]),
				TTL = 
				    s2i(TtlStr, 
					invalid_connection_data_conn_addr_ttl),
				?d("decode_pp_connection_data -> TTL: ~p", 
				   [TTL]),
				#megaco_sdp_c_conn_addr{base = Base,
							ttl  = TTL};
			    [Base] ->
				Base
			end,
		    ?d("decode_pp_connection_data -> "
		       "~n   ConnAddr: ~p", [ConnAddr]),
		    SDP = #megaco_sdp_c{network_type    = NT,
					address_type    = AT,
					connection_addr = ConnAddr},
		    {ok, SDP};
		ip6 ->
		    ?d("decode_pp_connection_data -> ip6", []),
		    SDP = #megaco_sdp_c{network_type    = NT,
					address_type    = AT,
					connection_addr = ConnectionAddr},
		    {ok, SDP};
		_ ->
		    SDP = #megaco_sdp_c{network_type    = NT,
					address_type    = AT,
					connection_addr = ConnectionAddr},
		    {ok, SDP}
	    end;
	Err ->
	    invalid_pp(connection_data, Value, Err)
    end.

encode_pp_connection_data(NetType0, AddrType0, ConnAddr0) ->
    ?d("encode_pp_connection_data -> entry with"
       "~n   NetType0:  ~p"
       "~n   AddrType0: ~p"
       "~n   ConnAddr0: ~p", [NetType0, AddrType0, ConnAddr0]),
    NetType  = encode_conn_data_network_type(NetType0),
    AddrType = encode_conn_data_address_type(AddrType0),
    ConnAddr = encode_conn_data_conn_addr(AddrType0, ConnAddr0),
    Val      = NetType ++ " " ++ AddrType ++ " " ++ ConnAddr,
    #'PropertyParm'{name  = "c", 
		    value = [Val]}.

encode_conn_data_network_type(NT) ->
    case (catch encode_network_type(NT)) of
	{error, _} ->
	    error({invalid_connection_data_network_type, NT});
	Val ->
	    Val
    end.

encode_conn_data_address_type(AT) ->
    case (catch encode_address_type(AT)) of
	{error, _} ->
	    error({invalid_connection_data_address_type, AT});
	Val ->
	    Val
    end.

encode_conn_data_conn_addr(_, CA) when is_list(CA) ->
    CA;
encode_conn_data_conn_addr(ip4, CA) 
  when is_record(CA, megaco_sdp_c_conn_addr) ->
    encode_conn_data_conn_addr(CA);
encode_conn_data_conn_addr(AT, CA) 
  when is_list(AT) and is_record(CA, megaco_sdp_c_conn_addr) ->
    case tolower(AT) of
	"ip4" ->
	    encode_conn_data_conn_addr(CA);
	_ ->
	    error({invalid_connection_data_conn_addr, {AT, CA}})
    end;
encode_conn_data_conn_addr(_, BadCA) ->
    error({invalid_connection_data_conn_addr, BadCA}).

encode_conn_data_conn_addr(#megaco_sdp_c_conn_addr{base   = Base0,
						   ttl    = TTL0,
						   num_of = undefined}) ->
    Base = encode_conn_data_conn_addr_base(Base0),
    TTL  = encode_conn_data_conn_addr_ttl(TTL0),
    Base ++ "/" ++ TTL;
encode_conn_data_conn_addr(#megaco_sdp_c_conn_addr{base   = Base0,
						   ttl    = TTL0,
						   num_of = NumOf0}) ->
    Base  = encode_conn_data_conn_addr_base(Base0),
    TTL   = encode_conn_data_conn_addr_ttl(TTL0),
    NumOf = encode_conn_data_conn_addr_num_of(NumOf0),
    Base ++ "/" ++ TTL ++ "/" ++ NumOf.
    
encode_conn_data_conn_addr_base(Base) when is_list(Base) -> 
    Base;
encode_conn_data_conn_addr_base(BadBase) ->
    error({invalid_connection_data_conn_addr_base, BadBase}).

encode_conn_data_conn_addr_ttl(TTL) when is_integer(TTL) -> 
    integer_to_list(TTL);
encode_conn_data_conn_addr_ttl(BadTTL) ->
    error({invalid_connection_data_conn_addr_ttl, BadTTL}).

encode_conn_data_conn_addr_num_of(NumOf) when is_integer(NumOf) -> 
    integer_to_list(NumOf);
encode_conn_data_conn_addr_num_of(BadNumOf) ->
    error({invalid_connection_data_conn_addr_num_of, BadNumOf}).


%% ===== Bandwidth =====
%% 
decode_pp_bandwidth(Value) ->
    ?d("decode_pp_bandwidth -> entry with"
       "~n   Value: ~p", [Value]),
    case string:tokens(Value, ":") of
	[BwTypeStr, BandwidthStr] ->
	    ?d("decode_pp_bandwidth -> "
	       "~n   BwTypeStr:    ~p"
	       "~n   BandwidthStr: ~p", [BwTypeStr, BandwidthStr]),
	    BwType    = decode_bandwidth_bwt(BwTypeStr),
	    ?d("decode_pp_bandwidth -> "
	       "~n   BwType: ~w", [BwType]),
	    Bandwidth = decode_bandwidth_bw(BandwidthStr),
	    ?d("decode_pp_bandwidth -> "
	       "~n   Bandwidth: ~w", [Bandwidth]),
	    SDP = #megaco_sdp_b{bwtype    = BwType,
				bandwidth = Bandwidth},
	    {ok, SDP};
	Err ->
	    invalid_pp(bandwidth_info, Value, Err)
    end.
    
encode_pp_bandwidth(BwType0, Bandwidth0) ->
    ?d("encode_pp_bandwidth -> entry with"
       "~n   BwType0:    ~p"
       "~n   Bandwidth0: ~p", [BwType0, Bandwidth0]),
    BwType    = encode_bandwidth_bwt(BwType0),
    Bandwidth = encode_bandwidth_bw(Bandwidth0), 
    Val       = BwType ++ ":" ++ Bandwidth, 
    #'PropertyParm'{name  = "b", 
		    value = [Val]}.

decode_bandwidth_bwt("CT") ->
    ct;
decode_bandwidth_bwt("AS") ->
    as;
decode_bandwidth_bwt(BwType) when is_list(BwType) ->
    BwType.

encode_bandwidth_bwt(ct) ->
    "CT";
encode_bandwidth_bwt(as) ->
    "AS";
encode_bandwidth_bwt(BwType) when is_list(BwType) ->
    BwType;
encode_bandwidth_bwt(BadBwType) ->
    error({invalid_bandwidth_bwtype, BadBwType}).

decode_bandwidth_bw(Bandwidth) ->
    s2i(Bandwidth, invalid_bandwidth_bandwidth).

encode_bandwidth_bw(Bandwidth) when is_integer(Bandwidth) ->
    integer_to_list(Bandwidth);
encode_bandwidth_bw(BadBandwidth) ->
    error({invalid_bandwidth_bandwidth, BadBandwidth}).


%% ===== Times =====
%% 
decode_pp_times(Value) ->
    ?d("decode_pp_times -> entry with"
       "~n   Value: ~p", [Value]),
    case string:tokens(Value, " \t") of
	[StartStr, StopStr] ->    
	    ?d("decode_pp_times -> "
	       "~n   StartStr: ~p"
	       "~n   StopStr:  ~p", [StartStr, StopStr]),
	    Start = decode_times_start(StartStr),
	    ?d("decode_pp_times -> entry with"
	       "~n   Stop: ~w", [Start]),
	    Stop  = decode_times_stop(StopStr),
	    ?d("decode_pp_times -> entry with"
	       "~n   Stop:  ~w", [Stop]),
	    SDP = #megaco_sdp_t{start = Start, 
				stop  = Stop},
	    {ok, SDP};
	Err ->
	    invalid_pp(times, Value, Err)
    end.

encode_pp_times(Start0, Stop0) ->
    ?d("encode_pp_times -> entry with"
       "~n   Start0:  ~p"
       "~n   Stop0:   ~p", [Start0, Stop0]),
    Start = encode_times_start(Start0), 
    Stop  = encode_times_stop(Stop0), 
    Val = Start ++ " " ++ Stop, 
    #'PropertyParm'{name  = "t", 
		    value = [Val]}.

decode_times_start(Time) ->
    s2i(Time, invalid_times_start).

encode_times_start(Time) when is_integer(Time) ->
    integer_to_list(Time);
encode_times_start(BadTime) ->
    error({invalid_times_start, BadTime}).
    
decode_times_stop(Time) ->
    s2i(Time, invalid_times_stop).

encode_times_stop(Time) when is_integer(Time) ->
    integer_to_list(Time);
encode_times_stop(BadTime) ->
    error({invalid_times_stop, BadTime}).
    

%% ===== Repeat Times =====
%% 
decode_pp_rtimes(Value) ->
    ?d("decode_pp_rtimes -> entry with"
       "~n   Value: ~p", [Value]),
    case string:tokens(Value, " \t") of
	[Repeat, Duration | ListOfOffsets] ->    
	    ?d("decode_pp_rtimes -> "
	       "~n   Repeat:        ~p"
	       "~n   Duration:      ~p"
	       "~n   ListOfOffsets: ~p", [Repeat, Duration, ListOfOffsets]),
	    SDP = #megaco_sdp_r{repeat_interval = Repeat,
				active_duration = Duration, 
				list_of_offsets = ListOfOffsets},
	    {ok, SDP};
	Err ->
	    invalid_pp(repeat_times, Value, Err)
    end.

encode_pp_rtimes(Repeat0, Duration0, ListOfOffsets0) ->
    ?d("encode_pp_rtimes -> entry with"
       "~n   Repeat0:        ~p"
       "~n   Duration0:      ~p"
       "~n   ListOfOffsets0: ~p", [Repeat0, Duration0, ListOfOffsets0]),
    Repeat        = encode_rtimes_repeat(Repeat0), 
    Duration      = encode_rtimes_duration(Duration0), 
    ListOfOffsets = encode_rtimes_list_of_offsets(ListOfOffsets0), 
    Val = Repeat ++ " " ++ Duration ++ ListOfOffsets,
    #'PropertyParm'{name  = "r", 
		    value = [Val]}.

encode_rtimes_repeat(Repeat) when is_list(Repeat) ->
    Repeat;
encode_rtimes_repeat(BadRepeat) ->
    error({invalid_rtimes_repeat, BadRepeat}).

encode_rtimes_duration(Duration) when is_list(Duration) ->
    Duration;
encode_rtimes_duration(BadDuration) ->
    error({invalid_rtimes_duration, BadDuration}).

encode_rtimes_list_of_offsets(LOO) when is_list(LOO) ->
    F = fun(Off, Acc) when is_list(Off) -> 
		Acc ++ " " ++ Off;
	   (BadOff, Acc) -> 
		error({invalid_rtimes_list_of_offsets, {BadOff, Acc}})
	end,
    lists:foldl(F, [], LOO);
encode_rtimes_list_of_offsets(BadLoo) ->
    error({invalid_rtimes_list_of_offsets, BadLoo}).
    
	    
%% ===== Time Zones =====
%% 
decode_pp_tzones(Value) when is_list(Value) and (length(Value) > 0) ->
    ?d("decode_pp_ztimes -> entry with"
       "~n   Value: ~p", [Value]),
    LOA = decode_tzones_list_of_adjustments(string:tokens(Value, " \t"), []),
    {ok, #megaco_sdp_z{list_of_adjustments = LOA}};
decode_pp_tzones(BadValue) ->
    error({invalid_tzones_list_of_adjustments, BadValue}).

encode_pp_tzones(LOA) ->
    ?d("encode_pp_ztimes -> entry with"
       "~n   LOA: ~p", [LOA]),
    Val = encode_tzones_list_of_adjustments(LOA),
    #'PropertyParm'{name  = "z", 
		    value = [Val]}.

decode_tzones_list_of_adjustments([], Acc) ->
    lists:reverse(Acc);
decode_tzones_list_of_adjustments([Adj], Acc) ->
    error({invalid_tzones_list_of_adjustments, Adj, lists:reverse(Acc)});
decode_tzones_list_of_adjustments([Time, Offset | LOA], Acc) ->
    Adj = #megaco_sdp_z_adjustement{time = Time, offset = Offset},
    decode_tzones_list_of_adjustments(LOA, [Adj | Acc]).

encode_tzones_list_of_adjustments([H|_] = LOA) 
  when is_record(H, megaco_sdp_z_adjustement) ->
    F = fun(#megaco_sdp_z_adjustement{time = T, offset = O}, Acc) -> 
		Acc ++ " " ++ T ++ " " ++ O;
	   (BadAdjustment, Acc) ->
		error({invalid_tzones_list_of_adjustments, 
		       {BadAdjustment, Acc}})
	end, 
    lists:foldl(F, [], LOA);
encode_tzones_list_of_adjustments(LOA) ->
    error({invalid_tzones_list_of_adjustments, LOA}).


%% ===== Encryption Keys =====
%% 
decode_pp_encryption_keys(Value) ->
    ?d("decode_pp_encryption_keys -> entry with"
       "~n   Value: ~p", [Value]),
    {M, E} = 
	case string:tokens(Value, ":") of
	    [Method, EncryptionKey] -> 
		?d("decode_pp_encryption_keys -> "
		   "~n   Method:        ~p"
		   "~n   EncryptionKey: ~p", [Method, EncryptionKey]),
		{Method, EncryptionKey};
	    [Method] -> 
		?d("decode_pp_encryption_keys -> "
		   "~n   Method: ~p", [Method]),
		{Method, undefined};
	    Err ->
		invalid_pp(encryption_key, Value, Err)
	end,
    M2 = 
	case tolower(M) of
	    "clear" ->
		clear;
	    "base64" ->
		base64;
	    "uri" ->
		uri;
	    "prompt" ->
		prompt;
	    _ ->
		M
	end,
    ?d("decode_pp_encryption_keys -> "
       "~n   M2: ~p", [M2]),
    SDP = #megaco_sdp_k{method         = M2, 
			encryption_key = E},
    {ok, SDP}.
    
encode_pp_encryption_keys(prompt = _Method, undefined) ->
    ?d("encode_pp_encryption_keys(prompt) -> entry", []),
    #'PropertyParm'{name  = "k", 
		    value = ["prompt"]};
encode_pp_encryption_keys(clear = _Method, EncryptionKey) 
  when is_list(EncryptionKey) ->
    ?d("encode_pp_encryption_keys(clear) -> entry with"
       "~n   EncryptionKey: ~p", [EncryptionKey]),
    #'PropertyParm'{name  = "k", 
		    value = ["clear:" ++ EncryptionKey]};
encode_pp_encryption_keys(base64 = _Method, EncryptionKey) 
  when is_list(EncryptionKey) ->
    ?d("encode_pp_encryption_keys(base64) -> entry with"
       "~n   EncryptionKey: ~p", [EncryptionKey]),
    #'PropertyParm'{name  = "k", 
		    value = ["base64:" ++ EncryptionKey]};
encode_pp_encryption_keys(uri = _Method, EncryptionKey) 
  when is_list(EncryptionKey) ->
    ?d("encode_pp_encryption_keys(uri) -> entry with"
       "~n   EncryptionKey: ~p", [EncryptionKey]),
    #'PropertyParm'{name  = "k", 
		    value = ["uri:" ++ EncryptionKey]};
encode_pp_encryption_keys(Method, EncryptionKey) 
  when is_list(Method) and is_list(EncryptionKey) ->
    ?d("encode_pp_encryption_keys -> entry with"
       "~n   Method:        ~p"
       "~n   EncryptionKey: ~p", [Method, EncryptionKey]),
    #'PropertyParm'{name  = "k", 
		    value = [Method ++ ":" ++ EncryptionKey]};
encode_pp_encryption_keys(BadMethod, BadEK) ->
    error({invalid_encryption_keys, {BadMethod, BadEK}}).

    
%% ===== Attributes =====
%% 
decode_pp_attribute(Value) ->
    ?d("decode_pp_attribute -> entry with"
       "~n   Value: ~p", [Value]),
    First = string:chr(Value, $:),
    if 
	(First > 0) and (First < length(Value)) ->
	    ?d("decode_pp_attribute -> value attribute", []),
	    Attr      = string:substr(Value, 1, First -1),
	    AttrValue = string:substr(Value, First + 1),
	    ?d("decode_pp_attribute -> "
	       "~n   Attr:      ~p"
	       "~n   AttrValue: ~p", [Attr, AttrValue]),
	    decode_pp_attribute_value(Attr, AttrValue);
	
	First > 0 ->
	    ?d("decode_pp_attribute -> value attribute (empty)", []),
	    Attr = string:substr(Value, 1, First -1),
	    ?d("decode_pp_attribute -> "
	       "~n   Attr: ~p", [Attr]),
	    decode_pp_attribute_value(Attr, []);
	    
	true ->
	    ?d("decode_pp_attribute -> binary attribute", []),
	    {ok, #megaco_sdp_a{attribute = Value}}

    end.

decode_pp_attribute_value("cat", AttrValue) ->
    ?d("decode_pp_attribute -> cat", []),
    SDP = #megaco_sdp_a_cat{category = AttrValue}, 
    {ok, SDP};

decode_pp_attribute_value("keywds", AttrValue) ->
    ?d("decode_pp_attribute -> keywds", []),
    SDP = #megaco_sdp_a_keywds{keywords = AttrValue}, 
    {ok, SDP};

decode_pp_attribute_value("tool", AttrValue) ->
    ?d("decode_pp_attribute -> tool", []),
    SDP = #megaco_sdp_a_tool{name_and_version = AttrValue}, 
    {ok, SDP};

decode_pp_attribute_value("ptime", AttrValue) ->
    ?d("decode_pp_attribute -> ptime", []),
    PacketTimeStr = string:strip(AttrValue, both, $ ), 
    PacketTime = 
	s2i(PacketTimeStr, invalid_ptime_packet_time), 
    ?d("decode_pp_attribute -> PacketTime: ~w", [PacketTime]),
    SDP = #megaco_sdp_a_ptime{packet_time = PacketTime}, 
    {ok, SDP};

decode_pp_attribute_value("maxptime", AttrValue) ->
    ?d("decode_pp_attribute -> maxptime", []),
    MaxPacketTimeStr = string:strip(AttrValue, both, $ ), 
    MaxPacketTime = 
	s2i(MaxPacketTimeStr, invalid_maxptime_maximum_packet_time), 
    ?d("decode_pp_attribute -> MaxPacketTime: ~w", [MaxPacketTime]),
    SDP = #megaco_sdp_a_maxptime{maximum_packet_time = MaxPacketTime}, 
    {ok, SDP};

decode_pp_attribute_value("rtpmap", AttrValue) ->
    ?d("decode_pp_attribute -> rtpmap", []),
    case string:tokens(AttrValue, "\/ \t") of
	[PayloadStr, EncName, ClockRateStr | EncPar] ->
	    ?d("decode_pp_attribute -> "
	       "~n   PayloadStr:   ~p"
	       "~n   EncName:      ~p"
	       "~n   ClockRateStr: ~p"
	       "~n   EncPar:       ~p", 
	       [PayloadStr, EncName, ClockRateStr, EncPar]),
		    Payload   = 
		s2i(PayloadStr, invalid_rtpmap_payload),
	    ?d("decode_pp_attribute -> Payload: ~w", 
	       [Payload]),
	    ClockRate = 
		s2i(ClockRateStr, invalid_rtpmap_payload), 
	    ?d("decode_pp_attribute -> ClockRate: ~w", 
	       [ClockRate]),
	    SDP = 
		#megaco_sdp_a_rtpmap{payload_type   = Payload, 
				     encoding_name  = EncName,
				     clock_rate     = ClockRate, 
				     encoding_parms = EncPar},
	    {ok, SDP};
	_ ->
	    error({invalid_rtpmap, AttrValue})
    end;

decode_pp_attribute_value("orient", AttrValue) ->
    ?d("decode_pp_attribute -> orient", []),
    Orientation = decode_attribute_orientation(AttrValue),
    SDP = #megaco_sdp_a_orient{orientation = Orientation}, 
    {ok, SDP};

decode_pp_attribute_value("type", AttrValue) ->
    ?d("decode_pp_attribute -> type", []),
    SDP = #megaco_sdp_a_type{conf_type = AttrValue}, 
    {ok, SDP};

decode_pp_attribute_value("charset", AttrValue) ->
    ?d("decode_pp_attribute -> charset", []),
    SDP = #megaco_sdp_a_charset{char_set = AttrValue}, 
    {ok, SDP};

decode_pp_attribute_value("sdplang", AttrValue) ->
    ?d("decode_pp_attribute -> sdplang", []),
    SDP = #megaco_sdp_a_sdplang{tag = AttrValue}, 
    {ok, SDP};

decode_pp_attribute_value("lang", AttrValue) ->
    ?d("decode_pp_attribute -> lang", []),
    SDP = #megaco_sdp_a_lang{tag = AttrValue}, 
    {ok, SDP};

decode_pp_attribute_value("framerate", AttrValue) ->
    ?d("decode_pp_attribute -> framerate", []),
    SDP = #megaco_sdp_a_framerate{frame_rate = AttrValue}, 
    {ok, SDP};

decode_pp_attribute_value("quality", AttrValue) ->
    ?d("decode_pp_attribute -> quality", []),
    QualityStr = AttrValue, 
    Quality = s2i(QualityStr, invalid_quality_quality), 
    ?d("decode_pp_attribute -> Quality: ~w", [Quality]),
    SDP = #megaco_sdp_a_quality{quality = Quality}, 
    {ok, SDP};

decode_pp_attribute_value("fmtp", AttrValue) ->
    ?d("decode_pp_attribute -> fmtp", []),
    FMTP = AttrValue, 
    First = string:chr(FMTP, $ ),
    if 
	(First > 0) and (First < length(FMTP)) ->
	    ?d("decode_pp_attribute_value -> valid fmtp with params", []),
	    Format = string:substr(FMTP, 1, First - 1),
	    Params = string:substr(FMTP, First + 1),
	    ?d("decode_pp_attribute_value -> "
	       "~n   Format: ~p"
	       "~n   Params: ~p", [Format, Params]),
	    SDP = #megaco_sdp_a_fmtp{format = Format,
				     param  = Params}, 
	    {ok, SDP};
	
	First > 0 ->
	    ?d("decode_pp_attribute -> valid fmtp", []),
	    Format = string:substr(FMTP, 1, First - 1),
	    ?d("decode_pp_attribute -> "
	       "~n   Format: ~p", [Format]),
	    {ok, #megaco_sdp_a_fmtp{format = Format, param = []}};
	    
	true ->
	    ?d("decode_pp_attribute_value -> no params", []),
	    {ok, #megaco_sdp_a_fmtp{format = FMTP, param = []}}

    end;

decode_pp_attribute_value(Attr, AttrValue) ->
    ?d("decode_pp_attribute -> unknown value attribute", []),
    {ok, #megaco_sdp_a{attribute = Attr, value = AttrValue}}.

decode_attribute_orientation("portrait") -> 
    portrait;
decode_attribute_orientation("landscape") ->
    landscape;
decode_attribute_orientation("seascape") ->
    seascape;
decode_attribute_orientation(BadOrientation) ->
    error({invalid_orient_orientation, BadOrientation}).
    
encode_attribute_orientation(portrait) -> 
    "portrait";
encode_attribute_orientation(landscape) ->
    "landscape";
encode_attribute_orientation(seascape) ->
    "seascape";
encode_attribute_orientation(BadOrientation) ->
    error({invalid_orient_orientation, BadOrientation}).
    

encode_pp_attribute_cat(Cat) when is_list(Cat) ->
    ?d("encode_pp_attribute_cat -> entry with"
       "~n   Cat: ~p", [Cat]),
    #'PropertyParm'{name  = "a", 
		    value = ["cat:" ++ Cat]};
encode_pp_attribute_cat(BadCat) ->
    error({invalid_cat_category, BadCat}).


encode_pp_attribute_keywds(Keywords) when is_list(Keywords) ->
    ?d("encode_pp_attribute_keywds -> entry with"
       "~n   Keywords: ~p", [Keywords]),
    #'PropertyParm'{name  = "a", 
		    value = ["keywds:" ++ Keywords]};
encode_pp_attribute_keywds(BadKeywords) ->
    error({invalid_keywds_keywords, BadKeywords}).


encode_pp_attribute_tool(NameAndVersion) when is_list(NameAndVersion) ->
    ?d("encode_pp_attribute_tool -> entry with"
       "~n   NameAndVersion: ~p", [NameAndVersion]),
    #'PropertyParm'{name  = "a", 
		    value = ["tool:" ++ NameAndVersion]};
encode_pp_attribute_tool(BadNameAndVersion) ->
    error({invalid_tool_name_and_version, BadNameAndVersion}).


encode_pp_attribute_ptime(PacketTime) when is_integer(PacketTime) ->
    ?d("encode_pp_attribute_ptime -> entry with"
       "~n   PacketTime: ~w", [PacketTime]),
    #'PropertyParm'{name  = "a", 
		    value = ["ptime:" ++ integer_to_list(PacketTime)]};
encode_pp_attribute_ptime(BadPT) ->
    error({invalid_ptime_packet_time, BadPT}).


encode_pp_attribute_maxptime(MaxPacketTime) when is_integer(MaxPacketTime) ->
    ?d("encode_pp_attribute_maxptime -> entry with"
       "~n   MaxPacketTime: ~w", [MaxPacketTime]),
    #'PropertyParm'{name  = "a", 
		    value = ["maxptime:" ++ integer_to_list(MaxPacketTime)]};
encode_pp_attribute_maxptime(BadMPT) ->
    error({invalid_maxptime_maximum_packet_time, BadMPT}).


encode_pp_attribute_rtpmap(Payload0, EncName0, ClockRate0, EncPar0) ->
    ?d("encode_pp_attribute_rtpmap -> entry with"
       "~n   Payload0:   ~p"
       "~n   EncName0:   ~p"
       "~n   ClockRate0: ~p"
       "~n   EncPar0:    ~p", [Payload0, EncName0, ClockRate0, EncPar0]),
    Payload   = encode_rtpmap_payload(Payload0), 
    EncName   = encode_rtpmap_encoding_name(EncName0), 
    ClockRate = encode_rtpmap_clockrate(ClockRate0), 
    EncPar    = encode_rtpmap_encoding_parms(EncPar0), 
    Val = "rtpmap:" ++ Payload ++ " " ++ 
	EncName ++ "/" ++ ClockRate ++ EncPar, 
    #'PropertyParm'{name  = "a", 
		    value = [Val]}.

encode_rtpmap_payload(Payload) when is_integer(Payload) ->
    integer_to_list(Payload);
encode_rtpmap_payload(BadPayload) ->
    error({invalid_rtpmap_payload, BadPayload}).

encode_rtpmap_encoding_name(EncName) when is_list(EncName) ->
    EncName;
encode_rtpmap_encoding_name(BadEncName) ->
    error({invalid_rtpmap_encoding_name, BadEncName}).

encode_rtpmap_clockrate(ClockRate) when is_integer(ClockRate) ->
    integer_to_list(ClockRate);
encode_rtpmap_clockrate(BadClockRate) ->
    error({invalid_rtpmap_clockrate, BadClockRate}).

encode_rtpmap_encoding_parms(EncPar) when is_list(EncPar) ->
    F = fun(EP, Acc) when is_list(EP) -> 
		Acc ++ "/" ++ EP;
	   (BadEP, Acc) ->
		error({invalid_rtpmap_encoding_parms, {BadEP, Acc}})
	end,
    lists:foldl(F, [], EncPar);
encode_rtpmap_encoding_parms(BadEncPar) ->
    error({invalid_rtpmap_encoding_parms, BadEncPar}).
		

encode_pp_attribute_orient(Orientation0) ->
    ?d("encode_pp_attribute_orient -> entry with"
       "~n   Orientation0: ~w", [Orientation0]),
    Orientation = encode_attribute_orientation(Orientation0), 
    #'PropertyParm'{name  = "a", 
		    value = ["orient:" ++ Orientation]}.


encode_pp_attribute_type(CType) when is_list(CType) ->
    ?d("encode_pp_attribute_type -> entry with"
       "~n   CType: ~p", [CType]),
    #'PropertyParm'{name  = "a", 
		    value = ["type:" ++ CType]};
encode_pp_attribute_type(BadCType) ->
    error({invalid_type_conf_type, BadCType}).


encode_pp_attribute_charset(CharSet) when is_list(CharSet) ->
    ?d("encode_pp_attribute_charset -> entry with"
       "~n   CharSet: ~p", [CharSet]),
    #'PropertyParm'{name  = "a", 
		    value = ["charset:" ++ CharSet]};
encode_pp_attribute_charset(BadCharSet) ->
    error({invalid_charset_char_set, BadCharSet}).


encode_pp_attribute_sdplang(SdpLang) when is_list(SdpLang) ->
    ?d("encode_pp_attribute_sdplang -> entry with"
       "~n   SdpLang: ~p", [SdpLang]),
    #'PropertyParm'{name  = "a", 
		    value = ["sdplang:" ++ SdpLang]};
encode_pp_attribute_sdplang(BadSdpLang) ->
    error({invalid_sdplang_tag, BadSdpLang}).


encode_pp_attribute_lang(Lang) when is_list(Lang) ->
    ?d("encode_pp_attribute_lang -> entry with"
       "~n   Lang: ~p", [Lang]),
    #'PropertyParm'{name  = "a", 
		    value = ["lang:" ++ Lang]};
encode_pp_attribute_lang(BadLang) ->
    error({invalid_lang_tag, BadLang}).


encode_pp_attribute_framerate(FrameRate) when is_list(FrameRate) ->
    ?d("encode_pp_attribute_framerate -> entry with"
       "~n   FrameRate: ~p", [FrameRate]),
    #'PropertyParm'{name  = "a", 
		    value = ["framerate:" ++ FrameRate]};
encode_pp_attribute_framerate(BadFrameRate) ->
    error({invalid_framerate_frame_rate, BadFrameRate}).


encode_pp_attribute_quality(Quality) when is_integer(Quality) ->
    ?d("encode_pp_attribute_quality -> entry with"
       "~n   Quality: ~w", [Quality]),
    #'PropertyParm'{name  = "a", 
		    value = ["quality:" ++ integer_to_list(Quality)]};
encode_pp_attribute_quality(BadQ) ->
    error({invalid_quality_quality, BadQ}).


encode_pp_attribute_fmtp(Fmt0, Params0) ->
    ?d("encode_pp_attribute_rtpmap -> entry with"
       "~n   Fmt0:    ~p"
       "~n   Params0: ~p", [Fmt0, Params0]),
    Fmt    = encode_fmtp_format(Fmt0),
    Params = encode_fmtp_param(Params0),
    Val    = "fmtp:" ++ Fmt ++ " " ++ Params, 
    #'PropertyParm'{name  = "a", 
		    value = [Val]}.

encode_fmtp_format(Fmt) when is_list(Fmt) ->
    Fmt;
encode_fmtp_format(BadFmt) ->
    error({invalid_fmtp_format, BadFmt}).

encode_fmtp_param(Param) when is_list(Param) ->
    Param;
encode_fmtp_param(BadParam) ->
    error({invalid_fmtp_param, BadParam}).


encode_pp_attribute(Attr, undefined) when is_list(Attr) ->
    ?d("encode_pp_attribute_rtpmap -> entry with"
       "~n   Attr: ~p", [Attr]),
    #'PropertyParm'{name  = "a", 
		    value = [Attr]};
encode_pp_attribute(Attr, Value) when is_list(Attr) and is_list(Value) ->
    ?d("encode_pp_attribute_rtpmap -> entry with"
       "~n   Attr:  ~p"
       "~n   Value: ~p", [Attr, Value]),
    #'PropertyParm'{name  = "a", 
		    value = [Attr ++ ":" ++ Value]};
encode_pp_attribute(BadAttr, BadAttrValue) ->
    error({invalid_attribute, {BadAttr, BadAttrValue}}).


%% ===== Media Announcements =====
%% 
decode_pp_media_announcement(Value) ->
    case string:tokens(Value, " \t") of
	[Media0, PortInfo, Transport | FMT] ->
	    Media = 
		case tolower(Media0) of
		    "audio"       -> audio;
		    "video"       -> video;
		    "application" -> application;
		    "data"        -> data;
		    "control"     -> control;
		    _             -> Media0
		end,
	    {Port, NoOfPorts} = 
		case string:tokens(PortInfo, "/") of
		    [P1, NP] ->
			{s2i(P1, invalid_media_announcement_port),
			 s2i(NP, invalid_media_announcement_nof_ports)};
		    [P2] ->
			{s2i(P2, invalid_media_announcement_port),
			 undefined};
		    Err ->
			invalid_pp(mnta_port_info, Value, Err)
		end,
	    SDP = #megaco_sdp_m{media     = Media,
				port      = Port,
				num_ports = NoOfPorts,
				transport = Transport,
				fmt_list  = FMT},
	    {ok, SDP};
	Err ->
	    invalid_pp(media_name_transp_addr, Value, Err)
    end.


encode_pp_media_announcement(Media0, Port0, undefined, Transport0, FMT0) ->
    ?d("encode_pp_media_announcement -> entry with"
       "~n   Media0:     ~p"
       "~n   Port0:      ~p"
       "~n   Transport0: ~p"
       "~n   FMT0:       ~p", [Media0, Port0, Transport0, FMT0]),
    Media     = encode_media_announcement_media(Media0),
    Port      = encode_media_announcement_port(Port0),
    Transport = encode_media_announcement_transport(Transport0),
    FMT       = encode_media_announcement_fmt_list(FMT0),
    do_encode_pp_media_announcement(Media, Port, "", Transport, FMT);
encode_pp_media_announcement(Media0, Port0, NumPorts0, Transport0, FMT0) ->
    ?d("encode_pp_media_announcement -> entry with"
       "~n   Media0:     ~p"
       "~n   Port0:      ~p"
       "~n   NumPorts0:  ~p"
       "~n   Transport0: ~p"
       "~n   FMT0:       ~p", [Media0, Port0, NumPorts0, Transport0, FMT0]),
    Media     = encode_media_announcement_media(Media0),
    Port      = encode_media_announcement_port(Port0),
    NumPorts  = encode_media_announcement_num_port(NumPorts0), 
    Transport = encode_media_announcement_transport(Transport0),
    FMT       = encode_media_announcement_fmt_list(FMT0),
    do_encode_pp_media_announcement(Media, Port, NumPorts, Transport, FMT).

do_encode_pp_media_announcement(Media, Port, NumOfPorts, Transport, FMT) ->
    Val = Media ++ " " ++ Port ++ NumOfPorts ++ " " ++ Transport ++ FMT,
    #'PropertyParm'{name  = "m", 
		    value = [Val]}.

encode_media_announcement_media(Media) when is_atom(Media) ->
    MaMedia = [audio, video, application, data, control], 
    case lists:member(Media, MaMedia) of
	true ->
	    atom_to_list(Media);
	false ->
	    error({invalid_media_announcement_media, Media})
    end;
encode_media_announcement_media(Media) when is_list(Media) ->
    Media;
encode_media_announcement_media(BadMedia) ->
    error({invalid_media_announcement_media, BadMedia}).

encode_media_announcement_port(Port) when is_integer(Port) ->
    integer_to_list(Port);
encode_media_announcement_port(BadPort) ->
    error({invalid_media_announcement_port, BadPort}).

encode_media_announcement_num_port(NumPort) when is_integer(NumPort) ->
    "/" ++ integer_to_list(NumPort);
encode_media_announcement_num_port(BadNumPort) ->
    error({invalid_media_announcement_num_port, BadNumPort}).

encode_media_announcement_transport(Transport) when is_list(Transport) ->
    Transport;
encode_media_announcement_transport(BadTransport) ->
    error({invalid_media_announcement_transport, BadTransport}).

encode_media_announcement_fmt_list(FmtList) when is_list(FmtList) ->
    F = fun(FMT, Acc) when is_list(FMT) -> 
		Acc ++ " " ++ FMT;
	   (BadFMT, Acc) ->
		error({invalid_media_announcement_fmt_list, {BadFMT, Acc}})
	end,
    lists:foldl(F, [], FmtList);
encode_media_announcement_fmt_list(BadFmtList) ->
    error({invalid_media_announcement_fmt_list, BadFmtList}).


decode_network_type(NT) when is_list(NT) ->
    case tolower(NT) of
	"in" -> in;
	_    -> NT
    end.

encode_network_type(in)                  -> "IN";
encode_network_type(NT) when is_list(NT) -> NT;
encode_network_type(Bad) ->
    {error, {invalid_network_type, Bad}}.


decode_address_type(AT) when is_list(AT) ->
    case tolower(AT) of
	"ip4" -> ip4;
	"ip6" -> ip6;
	_     -> AT
    end.
	     
encode_address_type(ip4)                 -> "IP4";
encode_address_type(ip6)                 -> "IP6";
encode_address_type(AT) when is_list(AT) -> 
    case toupper(AT) of
	"IP4" -> "IP4";
	"IP6" -> "IP6";
	_     -> AT
    end;
encode_address_type(Crap) ->
    {error, {invalid_address_type, Crap}}.
    

s2i(S, E) ->
    case (catch list_to_integer(S)) of
	I when is_integer(I) ->
	    I;
	_ ->
	    error({E, S})
    end.

-define(LOWER(Char),
        if
            Char >= $A, Char =< $Z ->
                Char - ($A - $a);
            true ->
                Char
        end).
tolower(Chars) ->
    [?LOWER(Char) || Char <- Chars].

-define(UPPER(Char),
        if
            Char >= $a, Char =< $z ->
                Char + ($A - $a);
            true ->
                Char
        end).
toupper(Chars) ->
    [?UPPER(Char) || Char <- Chars].
    
invalid_pp(What, Value, Error) ->
    {error, {invalid_PropertyParm, {What, Value, Error}}}.

error(Reason) ->
    throw({error, Reason}).

