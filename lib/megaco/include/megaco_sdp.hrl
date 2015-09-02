%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%%----------------------------------------------------------------------
%% Purpose: 
%%----------------------------------------------------------------------
%%
%%
%% Explanaition of the fields in the SDP body
%%   (See RFC 4566 for the complete decription)
%%
%% Session descriptions
%%
%%  v        protocol version
%%  o        owner/creator and session identifier 
%%  s        session name                          (optional)
%%  i        session information                   (optional)
%%  u        URI of description                    (optional)
%%  e        email address                         (optional)
%%  p        phone number                          (optional) 
%%  c        connection information                (optional) 
%%  b        bandwidth information                 (optional)
%%  One or more time descriptions ("t=" and "r=" lines; see below) 
%%  z        time zone adjustment                  (optional) 
%%  k        encryption key                        (optional) 
%%  a        zero or more session attribute lines  (optional) 
%%  Zero or more media descriptions
%%
%% Time descriptions
%%
%%  t        time the session is active
%%  r        zero or more repeat times             (optional)
%%  
%% Media descriptions, if present
%%
%%  m        media name and transport address
%%  i        media title                           (optional)
%%  c        connection information - optional if included at session-level
%%  b        bandwidth information                 (optional)
%%  k        encryption key                        (optional)
%%  a        zero or more media attribute lines    (optional)
%%
%%
%% An example SDP description is:
%%
%%      v=0
%%      o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5
%%      s=SDP Seminar
%%      i=A Seminar on the session description protocol
%%      u=http://www.example.com/seminars/sdp.pdf
%%      e=j.doe@example.com (Jane Doe)
%%      c=IN IP4 224.2.17.12/127
%%      t=2873397496 2873404696
%%      a=recvonly
%%      m=audio 49170 RTP/AVP 0
%%      m=video 51372 RTP/AVP 99
%%      a=rtpmap:99 h263-1998/90000
%% 
%% 
%%----------------------------------------------------------------------

-ifndef(megaco_sdp_).
-define(megaco_sdp_, true).


%% ===================================================================
%% 
%% Protocol Version ("v=")
%% 
%% v=0
%% 
%% The "v=" field gives the version of the Session Description 
%% Protocol. This memo defines version 0.  There is no minor version 
%% number
%% 

-record(megaco_sdp_v, {
	  version					% integer()
	 }
       ).




%% ===================================================================
%% 
%% Origin ("o=")
%%
%% o=<username> <sess-id> <sess-version> <nettype> <addrtype>
%% <unicast-address>
%%
%% The "o=" field gives the originator of the session (their username
%% and the address of the user's host) plus a session id and session
%% version number:
%%
%% <username> is the user's login on the originating host, or it is
%%    "-" if the originating host does not support the concept of 
%%    user IDs.  The <username> MUST NOT contain spaces.  
%% 
%% <sess-id> is a numeric string such that the tuple of <username>, 
%%    <sess-id>, <nettype>, <addrtype> and <unicast-address> form a 
%%    globally unique identifier for the session. The method of 
%%    <sess-id> allocation is up to the creating tool, but it has 
%%    been suggested that a Network Time Protocol (NTP)
%%    timestamp be used to ensure uniqueness [13].
%%
%% <sess-version> is a version number for this announcement.  Its
%%    usage is up to the creating tool, so long as <sess-version> 
%%    is increased when a modification is made to the session data.  
%%    Again, it is RECOMMENDED that an NTP format timestamp is used
%%
%% <nettype> is a text string giving the type of network.  
%%    Initially "IN" is defined to have the meaning "Internet", but 
%%    other values MAY be registered in the future (see Section 8 
%%    of RFC 4566).
%% 
%% <addrtype> is a text string giving the type of the address that
%%    follows.  Initially "IP4" and "IP6" are defined, but other 
%%    values MAY be registered in the future (see Section 8
%%    of RFC 4566).
%% 
%% <unicast-address> is the address of the machine from which the
%%    session was created.  For an address type of IP4, this is 
%%    either the fully qualified domain name of the machine or the 
%%    dotted-decimal representation of the IP version 4 address of 
%%    the machine. For an address type of IP6, this is either the 
%%    fully qualified domain name of the machine or the compressed 
%%    textual representation of the IP version 6 address of the 
%%    machine. For both IP4 and IP6, the fully qualified domain name 
%%    is the form that SHOULD be given unless this is unavailable, 
%%    in which case the globally unique address MAY be substituted.  
%%    A local IP address MUST NOT be used in any context where the 
%%    SDP description might leave the scope in which the address is 
%%    meaningful (for example, a local address MUST NOT be included 
%%    in an application-level referral that might leave the scope).
%%
%% In general, the "o=" field serves as a globally unique identifier
%% for this version of this session description, and the subfields
%% excepting the version taken together identify the session
%% irrespective of any modifications.
%% 
%% For privacy reasons, it is sometimes desirable to obfuscate the
%% username and IP address of the session originator.  If this is a
%% concern, an arbitrary <username> and private <unicast-address> MAY be
%% chosen to populate the "o=" field, provided that these are selected
%% in a manner that does not affect the global uniqueness of the field.
%% 
%% 

-record(megaco_sdp_o, {
          user_name,          % <username>        string()
          session_id,         % <sess-id>         integer()
          version,            % <sess-version>    integer()
          network_type = in,  % <nettype>         in | string()
          address_type = ip4, % <addrtype>        ip4 | ip6 | string()
          address             % <unicast-address> string()
         }
       ).


%% ===================================================================
%% 
%% Session Name ("s=")
%%
%% s=<session name>
%%
%% The "s=" field is the textual session name.  There MUST be one and
%% only one "s=" field per session description.  The "s=" field MUST 
%% NOT be empty and SHOULD contain ISO 10646 characters (but see also 
%% the "a=charset" attribute).  If a session has no meaningful name, 
%% the value "s= " SHOULD be used (i.e., a single space as the session
%% name).
%% 

-record(megaco_sdp_s, {
	  name                                  % string()
	 }
       ).




%% ===================================================================
%% 
%% Session and Media Information ("i=")
%%
%% i=<session description>
%%
%% The "i=" field provides textual information about the session.  
%% There MUST be at most one session-level "i=" field per session 
%% description, and at most one "i=" field per media.  If the 
%% "a=charset" attribute is present, it specifies the character set 
%% used in the "i=" field. If the "a=charset" attribute is not 
%% present, the "i=" field MUST contain ISO 10646 characters in 
%% UTF-8 encoding.
%%
%% A single "i=" field MAY also be used for each media definition.  
%% In media definitions, "i=" fields are primarily intended for 
%% labelling media streams.  As such, they are most likely to be 
%% useful when a single session has more than one distinct media 
%% stream of the same media type.  An example would be two different 
%% whiteboards, one for slides and one for feedback and questions.
%%
%% The "i=" field is intended to provide a free-form human-readable
%% description of the session or the purpose of a media stream.  It 
%% is not suitable for parsing by automata.
%% 

-record(megaco_sdp_i, {
	  session_descriptor                    % string()
	 }
       ).




%% ===================================================================
%% 
%% URI ("u=")
%%
%% u=<URI>
%%
%% A URI is a Uniform Resource Identifier as used by WWW clients [7].
%% The URI should be a pointer to additional information about the
%% session.  This field is OPTIONAL, but if it is present it MUST be
%% specified before the first media field.  No more than one URI field
%% is allowed per session description.
%% 

-record(megaco_sdp_u, {
	  uri                                   % string()
	 }
       ).




%% ===================================================================
%% 
%% Email Address and Phone Number ("e=" and "p=")
%%
%% e=<email address>
%% p=<phone number>
%%
%% The "e=" and "p=" lines specify contact information for the person
%% responsible for the conference.  This is not necessarily the same
%% person that created the conference announcement.
%%
%% Inclusion of an email address or phone number is OPTIONAL.  Note 
%% that the previous version of SDP specified that either an email 
%% field or a phone field MUST be specified, but this was widely 
%% ignored.  The change brings the specification into line with 
%% common usage.
%%
%% If an email address or phone number is present, it MUST be 
%% specified before the first media field.  More than one email or 
%% phone field can be given for a session description.
%%
%% Phone numbers SHOULD be given in the form of an international 
%% public telecommunication number (see ITU-T Recommendation E.164) 
%% preceded by a "+".  Spaces and hyphens may be used to split up a 
%% phone field to aid readability if desired.  For example:
%%
%%    p=+1 617 555-6011
%%
%% Both email addresses and phone numbers can have an OPTIONAL free 
%% text string associated with them, normally giving the name of the 
%% person who may be contacted.  This MUST be enclosed in parentheses 
%% if it is present.  For example:
%%
%%    e=j.doe@example.com (Jane Doe)
%%
%% The alternative RFC 2822 [29] name quoting convention is also 
%% allowed for both email addresses and phone numbers.  For example:
%%
%%    e=Jane Doe <j.doe@example.com>
%% 
%% The free text string SHOULD be in the ISO-10646 character set with
%% UTF-8 encoding, or alternatively in ISO-8859-1 or other encodings 
%% if the appropriate session-level "a=charset" attribute is set.
%% 

-record(megaco_sdp_e, {
	  email                                 % string()
	 }
       ).

-record(megaco_sdp_p, {
	  phone_number                          % string()
	 }
       ).




%% ===================================================================
%% 
%% Connection Data ("c=")
%% 
%% c=<nettype> <addrtype> <connection-address>
%%
%% The "c=" field contains connection data.
%%
%% A session description MUST contain either at least one "c=" field 
%% in each media description or a single "c=" field at the session 
%% level. It MAY contain a single session-level "c=" field and 
%% additional "c=" field(s) per media description, in which case the 
%% per-media values override the session-level settings for the 
%% respective media.
%%
%% The first sub-field ("<nettype>") is the network type, which is a
%% text string giving the type of network.  Initially, "IN" is 
%% defined to have the meaning "Internet", but other values MAY be 
%% registered in the future (see Section 8 of RFC 4566).
%%
%% The second sub-field ("<addrtype>") is the address type.  This 
%% allows SDP to be used for sessions that are not IP based. This 
%% memo only defines IP4 and IP6, but other values MAY be registered 
%% in the future (see Section 8 of RFC 4566).
%%
%% The third sub-field ("<connection-address>") is the connection
%% address.  OPTIONAL sub-fields MAY be added after the connection
%% address depending on the value of the <addrtype> field.
%%
%% When the <addrtype> is IP4 and IP6, the connection address is 
%% defined as follows:
%%
%% o  If the session is multicast, the connection address will be an 
%%    IP multicast group address.  If the session is not multicast, 
%%    then the connection address contains the unicast IP address of 
%%    the expected data source or data relay or data sink as 
%%    determined by additional attribute fields.  It is not expected 
%%    that unicast addresses will be given in a session description 
%%    that is communicated by a multicast announcement, though this 
%%    is not prohibited.
%%
%% o  Sessions using an IPv4 multicast connection address MUST also 
%%    have a time to live (TTL) value present in addition to the 
%%    multicast address.  The TTL and the address together define the 
%%    scope with which multicast packets sent in this conference will 
%%    be sent. TTL values MUST be in the range 0-255.  Although the 
%%    TTL MUST be specified, its use to scope multicast traffic is 
%%    deprecated; applications SHOULD use an administratively scoped 
%%    address instead.
%%
%% The TTL for the session is appended to the address using a slash 
%% as a separator.  An example is:
%%
%%    c=IN IP4 224.2.36.42/127
%%
%% IPv6 multicast does not use TTL scoping, and hence the TTL value 
%% MUST NOT be present for IPv6 multicast.  It is expected that IPv6 
%% scoped addresses will be used to limit the scope of conferences.
%%
%% Hierarchical or layered encoding schemes are data streams where 
%% the encoding from a single media source is split into a number of 
%% layers. The receiver can choose the desired quality (and hence 
%% bandwidth) by only subscribing to a subset of these layers.  Such 
%% layered encodings are normally transmitted in multiple multicast 
%% groups to allow multicast pruning.  This technique keeps unwanted 
%% traffic from sites only requiring certain levels of the hierarchy.  
%% For applications requiring multiple multicast groups, we allow the 
%% following notation to be used for the connection address:
%%
%%    <base multicast address>[/<ttl>]/<number of addresses>
%%
%% If the number of addresses is not given, it is assumed to be one.
%% Multicast addresses so assigned are contiguously allocated above 
%% the base address, so that, for example:
%%
%%    c=IN IP4 224.2.1.1/127/3
%%
%% would state that addresses 224.2.1.1, 224.2.1.2, and 224.2.1.3 are 
%% to be used at a TTL of 127.  This is semantically identical to 
%% including multiple "c=" lines in a media description:
%%
%%    c=IN IP4 224.2.1.1/127
%%    c=IN IP4 224.2.1.2/127
%%    c=IN IP4 224.2.1.3/127
%%
%% Similarly, an IPv6 example would be:
%%
%%    c=IN IP6 FF15::101/3
%%
%% which is semantically equivalent to:
%%
%%    c=IN IP6 FF15::101
%%    c=IN IP6 FF15::102
%%    c=IN IP6 FF15::103
%%
%% (remembering that the TTL field is not present in IPv6 multicast).
%%
%% Multiple addresses or "c=" lines MAY be specified on a per-media
%% basis only if they provide multicast addresses for different layers
%% in a hierarchical or layered encoding scheme.  They MUST NOT be
%% specified for a session-level "c=" field.
%%
%% The slash notation for multiple addresses described above MUST NOT 
%% be used for IP unicast addresses.
%% 

-record(megaco_sdp_c, {
          network_type = in,   % <nettype>             in | string()
          address_type = ip4,  % <addrtype>            ip4 | ip6 | string()
          connection_addr      % <connection-address>  string() | conn_addr()
         }).

%% Only if address type = ip4
%% conn_addr() -> #megaco_sdp_c_conn_addr{}
-record(megaco_sdp_c_conn_addr, {
	  base,                % <base multicast address> string()
	  ttl,                 % <ttl>                    integer()
	  num_of               % <number of addresses>    undefined | integer()
	 }).



%% ===================================================================
%% 
%% Bandwidth ("b=")
%%
%% b=<bwtype>:<bandwidth>
%%
%% This OPTIONAL field denotes the proposed bandwidth to be used by 
%% the session or media.  The <bwtype> is an alphanumeric modifier 
%% giving the meaning of the <bandwidth> figure.  Two values are 
%% defined in this specification, but other values MAY be registered 
%% in the future (see Section 8 of RFC 4566 and [21], [25]):
%%
%% CT If the bandwidth of a session or media in a session is 
%%    different from the bandwidth implicit from the scope, a 
%%    "b=CT:..." line SHOULD be supplied for the session giving the 
%%    proposed upper limit to the bandwidth used (the "conference 
%%    total" bandwidth).  The primary purpose of this is to give an 
%%    approximate idea as to whether two or more sessions can coexist 
%%    simultaneously.  When using the CT modifier with RTP, if 
%%    several RTP sessions are part of the conference, the conference 
%%    total refers to total bandwidth of all RTP sessions.
%%
%% AS The bandwidth is interpreted to be application specific (it 
%%    will be the application's concept of maximum bandwidth).  
%%    Normally, this will coincide with what is set on the 
%%    application's "maximum bandwidth" control if applicable.  For 
%%    RTP-based applications, AS gives the RTP "session bandwidth" 
%%    as defined in Section 6.2 of [19].
%%
%% Note that CT gives a total bandwidth figure for all the media at 
%% all sites.  AS gives a bandwidth figure for a single media at a 
%% single site, although there may be many sites sending 
%% simultaneously.
%%
%% A prefix "X-" is defined for <bwtype> names.  This is intended 
%% for experimental purposes only.  For example:
%%
%%    b=X-YZ:128
%%
%% Use of the "X-" prefix is NOT RECOMMENDED: instead new modifiers
%% SHOULD be registered with IANA in the standard namespace.  SDP
%% parsers MUST ignore bandwidth fields with unknown modifiers.
%% Modifiers MUST be alphanumeric and, although no length limit is
%% given, it is recommended that they be short.
%%
%% The <bandwidth> is interpreted as kilobits per second by default.
%% The definition of a new <bwtype> modifier MAY specify that the
%% bandwidth is to be interpreted in some alternative unit (the "CT" 
%% and "AS" modifiers defined in this memo use the default units).
%% 

%% bwtype() -> ct | as | string()
-record(megaco_sdp_b, {
          bwtype,                               % bwtype()
          bandwidth                             % integer()
         }).




%% ===================================================================
%% 
%% Times ("t=")
%%
%% t=<start time>  <stop time>
%%
%% The "t=" lines specify the start and stop times for a session.
%% Multiple "t=" lines MAY be used if a session is active at 
%% multiple irregularly spaced times; each additional "t=" line 
%% specifies an additional period of time for which the session will 
%% be active.  If the session is active at regular times, an "r=" 
%% line (see below) should be used in addition to, and following, a 
%% "t=" line -- in which case the "t=" line specifies the start and 
%% stop times of the repeat sequence.
%%
%% The first and second sub-fields give the start and stop times,
%% respectively, for the session.  These values are the decimal
%% representation of Network Time Protocol (NTP) time values in 
%% seconds since 1900 [13].  To convert these values to UNIX time, 
%% subtract decimal 2208988800.
%%
%% NTP timestamps are elsewhere represented by 64-bit values, which 
%% wrap sometime in the year 2036.  Since SDP uses an arbitrary length
%% decimal representation, this should not cause an issue (SDP
%% timestamps MUST continue counting seconds since 1900, NTP will use
%% the value modulo the 64-bit limit).
%%
%% If the <stop-time> is set to zero, then the session is not bounded,
%% though it will not become active until after the <start-time>.  If
%% the <start-time> is also zero, the session is regarded as 
%% permanent.
%%
%% User interfaces SHOULD strongly discourage the creation of 
%% unbounded and permanent sessions as they give no information about 
%% when the session is actually going to terminate, and so make 
%% scheduling difficult.
%%
%% The general assumption may be made, when displaying unbounded
%% sessions that have not timed out to the user, that an unbounded
%% session will only be active until half an hour from the current 
%% time or the session start time, whichever is the later.  If 
%% behaviour other than this is required, an end-time SHOULD be given 
%% and modified as appropriate when new information becomes available 
%% about when the session should really end.
%%
%% Permanent sessions may be shown to the user as never being active
%% unless there are associated repeat times that state precisely when
%% the session will be active.
%% 

-record(megaco_sdp_t, {
          start,                                % integer()
          stop                                  % integer()
         }).



%% ===================================================================
%% 
%% Repeat Times ("r=")
%% 
%% r=<repeat interval> <active duration> <offsets from start-time>
%% 
%% "r=" fields specify repeat times for a session.  For example, if a
%% session is active at 10am on Monday and 11am on Tuesday for one 
%% hour each week for three months, then the <start-time> in the
%% corresponding "t=" field would be the NTP representation of 10am 
%% on the first Monday, the <repeat interval> would be 1 week, the 
%% <active duration> would be 1 hour, and the offsets would be zero 
%% and 25 hours.  The corresponding "t=" field stop time would be 
%% the NTP representation of the end of the last session three months 
%% later.  By default, all fields are in seconds, so the "r=" and 
%% "t=" fields might be the following:
%%
%%    t=3034423619 3042462419
%%    r=604800 3600 0 90000
%%
%% To make description more compact, times may also be given in units 
%% of days, hours, or minutes.  The syntax for these is a number
%% immediately followed by a single case-sensitive character.
%% Fractional units are not allowed -- a smaller unit should be used
%% instead.  The following unit specification characters are allowed:
%%
%%    d - days (86400 seconds)
%%    h - hours (3600 seconds)
%%    m - minutes (60 seconds)
%%    s - seconds (allowed for completeness)
%%
%% Thus, the above session announcement could also have been written:
%%
%%    r=7d 1h 0 25h
%%
%% Monthly and yearly repeats cannot be directly specified with a 
%% single SDP repeat time; instead, separate "t=" fields should be 
%% used to explicitly list the session times.
%% 

-record(megaco_sdp_r, {
          repeat_interval,                      % string()
          active_duration,                      % string()
	  list_of_offsets                       % [ string() ]
	 }
       ).
      


%% ===================================================================
%% 
%% Time Zones ("z=")
%% 
%% z=<adjustment time> <offset> <adjustment time> <offset> ....
%% 
%% To schedule a repeated session that spans a change from daylight
%% saving time to standard time or vice versa, it is necessary to
%% specify offsets from the base time.  This is required because
%% different time zones change time at different times of day, 
%% different countries change to or from daylight saving time on 
%% different dates, and some countries do not have daylight saving 
%% time at all.
%%
%% Thus, in order to schedule a session that is at the same time 
%% winter and summer, it must be possible to specify unambiguously by 
%% whose time zone a session is scheduled.  To simplify this task for
%% receivers, we allow the sender to specify the NTP time that a time
%% zone adjustment happens and the offset from the time when the 
%% session was first scheduled.  The "z=" field allows the sender to 
%% specify a list of these adjustment times and offsets from the base 
%% time.
%%
%% An example might be the following:
%%
%%    z=2882844526 -1h 2898848070 0
%%
%% This specifies that at time 2882844526, the time base by which the
%% session's repeat times are calculated is shifted back by 1 hour, 
%% and that at time 2898848070, the session's original time base is
%% restored.  Adjustments are always relative to the specified start
%% time -- they are not cumulative.  Adjustments apply to all "t=" 
%% and "r=" lines in a session description.
%%
%% If a session is likely to last several years, it is expected that 
%% the session announcement will be modified periodically rather than
%% transmit several years' worth of adjustments in one session
%% announcement.
%% 

%% adjustment() -> #megaco_sdp_z_adjustement{}
-record(megaco_sdp_z, {
	  list_of_adjustments                   % [ adjustment() ]
	 }
       ).

-record(megaco_sdp_z_adjustement, {
	  time,         % string()
	  offset        % string()
	 }
       ).




%% ===================================================================
%% 
%% Encryption Keys ("k=")
%% 
%% k=<method>
%% k=<method>:<encryption key>
%%
%% If transported over a secure and trusted channel, the Session
%% Description Protocol MAY be used to convey encryption keys.  A 
%% simple mechanism for key exchange is provided by the key field 
%% ("k="), although this is primarily supported for compatibility 
%% with older implementations and its use is NOT RECOMMENDED.  Work 
%% is in progress to define new key exchange mechanisms for use with 
%% SDP [27] [28], and it is expected that new applications will use 
%% those mechanisms. A key field is permitted before the first media 
%% entry (in which case it applies to all media in the session), or 
%% for each media entry as required.  The format of keys and their 
%% usage are outside the scope of this document, and the key field 
%% provides no way to indicate the encryption algorithm to be used, 
%% key type, or other information about the key: this is assumed to 
%% be provided by the higher-level protocol using SDP.  If there is 
%% a need to convey this information within SDP, the extensions 
%% mentioned previously SHOULD be used.  Many security protocols 
%% require two keys: one for confidentiality, another for integrity.  
%% This specification does not support transfer of two keys.
%% 
%% The method indicates the mechanism to be used to obtain a usable 
%% key by external means, or from the encoded encryption key given.  
%% The following methods are defined:
%%
%%    k=clear:<encryption key>
%%
%%       The encryption key is included untransformed in this key 
%%       field. This method MUST NOT be used unless it can be 
%%       guaranteed that the SDP is conveyed over a secure channel.  
%%       The encryption key is interpreted as text according to the 
%%       charset attribute; use the "k=base64:" method to convey 
%%       characters that are otherwise prohibited in SDP.
%%
%%    k=base64:<encoded encryption key>
%%
%%       The encryption key is included in this key field but has 
%%       been base64 encoded [12] because it includes characters 
%%       that are prohibited in SDP.  This method MUST NOT be used 
%%       unless it can be guaranteed that the SDP is conveyed over 
%%       a secure channel.
%%
%%    k=uri:<URI to obtain key>
%%
%%       A Uniform Resource Identifier is included in the key field.
%%       The URI refers to the data containing the key, and may 
%%       require additional authentication before the key can be 
%%       returned.  When a request is made to the given URI, the 
%%       reply should specify the encoding for the key.  The URI is 
%%       often an Secure Socket Layer/Transport Layer Security 
%%       (SSL/TLS)-protected HTTP URI ("https:"), although this is 
%%       not required.
%%
%%    k=prompt
%%
%%       No key is included in this SDP description, but the session 
%%       or media stream referred to by this key field is encrypted.  
%%       The user should be prompted for the key when attempting to 
%%       join the session, and this user-supplied key should then be 
%%       used to decrypt the media streams.  The use of 
%%       user-specified keys is NOT RECOMMENDED, since such keys tend 
%%       to have weak security properties.
%%
%% The key field MUST NOT be used unless it can be guaranteed that 
%% the SDP is conveyed over a secure and trusted channel.  An example 
%% of such a channel might be SDP embedded inside an S/MIME message 
%% or a TLS-protected HTTP session.  It is important to ensure that 
%% the secure channel is with the party that is authorised to join the
%% session, not an intermediary: if a caching proxy server is used, it
%% is important to ensure that the proxy is either trusted or unable 
%% to access the SDP.
%% 

%% method() -> clear | base64 | uri | prompt
-record(megaco_sdp_k, {
	  method,            % method() | string()
	  encryption_key     % undefined | string()
	 }
       ).


%% ===================================================================
%% 
%% Attributes ("a=")
%% 
%% a=<attribute>
%% a=<attribute>:<value>
%%
%% Attributes are the primary means for extending SDP.  Attributes 
%% may be defined to be used as "session-level" attributes, 
%% "media-level" attributes, or both.
%%
%% A media description may have any number of attributes ("a=" 
%% fields) that are media specific.  These are referred to as 
%% "media-level" attributes and add information about the media 
%% stream.  Attribute fields can also be added before the first 
%% media field; these "session-level" attributes convey additional 
%% information that applies to the conference as a whole rather than 
%% to individual media.
%%
%% Attribute fields may be of two forms:
%%
%% o  A property attribute is simply of the form "a=<flag>".  These 
%%    are binary attributes, and the presence of the attribute 
%%    conveys that the attribute is a property of the session.  An 
%%    example might be "a=recvonly".
%%
%% o  A value attribute is of the form "a=<attribute>:<value>".  For
%%    example, a whiteboard could have the value attribute "a=orient:
%%    landscape"
%%
%% Attribute interpretation depends on the media tool being invoked.
%% Thus receivers of session descriptions should be configurable in
%% their interpretation of session descriptions in general and of
%% attributes in particular.
%%
%% Attribute names MUST use the US-ASCII subset of ISO-10646/UTF-8.
%%
%% Attribute values are octet strings, and MAY use any octet value
%% except 0x00 (Nul), 0x0A (LF), and 0x0D (CR).  By default, 
%% attribute values are to be interpreted as in ISO-10646 character 
%% set with UTF-8 encoding.  Unlike other text fields, attribute 
%% values are NOT normally affected by the "charset" attribute as 
%% this would make comparisons against known values problematic.  
%% However, when an attribute is defined, it can be defined to be 
%% charset dependent, in which case its value should be interpreted 
%% in the session charset rather than in ISO-10646.
%%
%% Attributes MUST be registered with IANA (see Section 8 of RFC 
%% 4566).  If an attribute is received that is not understood, it 
%% MUST be ignored by the receiver.
%% 
%% SDP Attributes
%% 
%% The following attributes are defined.  Since application writers 
%% may add new attributes as they are required, this list is not 
%% exhaustive. Registration procedures for new attributes are defined 
%% in Section 8.2.4 of RFC 4566.
%%
%%    a=cat:<category>
%%
%%       This attribute gives the dot-separated hierarchical category 
%%       of the session.  This is to enable a receiver to filter 
%%       unwanted sessions by category.  There is no central registry 
%%       of categories.  It is a session-level attribute, and it is 
%%       not dependent on charset.
%%
%%    a=keywds:<keywords>
%%
%%       Like the cat attribute, this is to assist identifying wanted
%%       sessions at the receiver.  This allows a receiver to select
%%       interesting session based on keywords describing the purpose 
%%       of the session; there is no central registry of keywords.  It 
%%       is a session-level attribute.  It is a charset-dependent 
%%       attribute, meaning that its value should be interpreted in the 
%%       charset specified for the session description if one is 
%%       specified,  or by default in ISO 10646/UTF-8.
%%
%%    a=tool:<name and version of tool>
%%
%%       This gives the name and version number of the tool used to
%%       create the session description.  It is a session-level
%%       attribute, and it is not dependent on charset.
%%
%%    a=ptime:<packet time>
%%
%%       This gives the length of time in milliseconds represented by
%%       the media in a packet.  This is probably only meaningful for
%%       audio data, but may be used with other media types if it 
%%       makes sense.  It should not be necessary to know ptime to 
%%       decode RTP or vat audio, and it is intended as a 
%%       recommendation for the encoding/packetisation of audio.  It 
%%       is a media-level attribute, and it is not dependent on charset.
%%
%%    a=maxptime:<maximum packet time>
%%
%%       This gives the maximum amount of media that can be encapsulated
%%       in each packet, expressed as time in milliseconds.  The time
%%       SHALL be calculated as the sum of the time the media present in
%%       the packet represents.  For frame-based codecs, the time SHOULD
%%       be an integer multiple of the frame size.  This attribute is
%%       probably only meaningful for audio data, but may be used with
%%       other media types if it makes sense.  It is a media-level
%%       attribute, and it is not dependent on charset.  Note that this
%%       attribute was introduced after RFC 2327, and non-updated
%%       implementations will ignore this attribute.
%%
%%    a=rtpmap:<payload type> <encoding name>/<clock rate> [/<encoding
%%       parameters>]
%%
%%       This attribute maps from an RTP payload type number (as used in
%%       an "m=" line) to an encoding name denoting the payload format
%%       to be used.  It also provides information on the clock rate and
%%       encoding parameters.  It is a media-level attribute that is not
%%       dependent on charset.
%%
%%       Although an RTP profile may make static assignments of payload
%%       type numbers to payload formats, it is more common for that
%%       assignment to be done dynamically using "a=rtpmap:" attributes.
%%       As an example of a static payload type, consider u-law PCM
%%       coded single-channel audio sampled at 8 kHz.  This is
%%       completely defined in the RTP Audio/Video profile as payload
%%       type 0, so there is no need for an "a=rtpmap:" attribute, and
%%       the media for such a stream sent to UDP port 49232 can be
%%       specified as:
%%
%%          m=audio 49232 RTP/AVP 0
%%
%%       An example of a dynamic payload type is 16-bit linear encoded
%%       stereo audio sampled at 16 kHz.  If we wish to use the dynamic
%%       RTP/AVP payload type 98 for this stream, additional 
%%       information is required to decode it:
%%
%%          m=audio 49232 RTP/AVP 98
%%          a=rtpmap:98 L16/16000/2
%%
%%       Up to one rtpmap attribute can be defined for each media 
%%       format specified.  Thus, we might have the following:
%%
%%          m=audio 49230 RTP/AVP 96 97 98
%%          a=rtpmap:96 L8/8000
%%          a=rtpmap:97 L16/8000
%%          a=rtpmap:98 L16/11025/2
%%
%%       RTP profiles that specify the use of dynamic payload types 
%%       MUST define the set of valid encoding names and/or a means to
%%       register encoding names if that profile is to be used with 
%%       SDP. The "RTP/AVP" and "RTP/SAVP" profiles use media subtypes 
%%       for encoding names, under the top-level media type denoted in 
%%       the "m=" line.  In the example above, the media types are
%%       "audio/l8" and "audio/l16".
%%
%%       For audio streams, <encoding parameters> indicates the number
%%       of audio channels.  This parameter is OPTIONAL and may be
%%       omitted if the number of channels is one, provided that no
%%       additional parameters are needed.
%%
%%       For video streams, no encoding parameters are currently
%%       specified.
%%
%%       Additional encoding parameters MAY be defined in the future,
%%       but codec-specific parameters SHOULD NOT be added.  
%%       Parameters added to an "a=rtpmap:" attribute SHOULD only be 
%%       those required for a session directory to make the choice of 
%%       appropriate media to participate in a session.  Codec-specific 
%%       parameters should be added in other attributes (for example, 
%%       "a=fmtp:").
%%
%%       Note: RTP audio formats typically do not include information
%%       about the number of samples per packet.  If a non-default (as
%%       defined in the RTP Audio/Video Profile) packetisation is
%%       required, the "ptime" attribute is used as given above.
%%
%%    a=recvonly
%%
%%       This specifies that the tools should be started in 
%%       receive-only mode where applicable.  It can be either a 
%%       session- or media-level attribute, and it is not dependent 
%%       on charset.  Note that recvonly applies to the media only, 
%%       not to any  associated control protocol (e.g., an RTP-based 
%%       system in recvonly mode SHOULD still send RTCP packets).
%%
%%    a=sendrecv
%%
%%       This specifies that the tools should be started in send and
%%       receive mode.  This is necessary for interactive conferences
%%       with tools that default to receive-only mode.  It can be 
%%       either a session or media-level attribute, and it is not 
%%       dependent on charset.
%%
%%       If none of the attributes "sendonly", "recvonly", "inactive",
%%       and "sendrecv" is present, "sendrecv" SHOULD be assumed as 
%%       the default for sessions that are not of the conference type
%%       "broadcast" or "H332" (see below).
%%
%%    a=sendonly
%%
%%       This specifies that the tools should be started in send-only
%%       mode.  An example may be where a different unicast address is
%%       to be used for a traffic destination than for a traffic 
%%       source. In such a case, two media descriptions may be used, 
%%       one sendonly and one recvonly.  It can be either a session- 
%%       or media-level attribute, but would normally only be used as 
%%       a media attribute.  It is not dependent on charset.  Note 
%%       that sendonly applies only to the media, and any associated 
%%       control protocol (e.g., RTCP) SHOULD still be received and 
%%       processed as normal.
%%
%%    a=inactive
%%
%%       This specifies that the tools should be started in inactive
%%       mode.  This is necessary for interactive conferences where
%%       users can put other users on hold.  No media is sent over an
%%       inactive media stream.  Note that an RTP-based system SHOULD
%%       still send RTCP, even if started inactive.  It can be either 
%%       a session or media-level attribute, and it is not dependent 
%%       on charset.
%%
%%    a=orient:<orientation>
%%
%%       Normally this is only used for a whiteboard or presentation
%%       tool.  It specifies the orientation of a the workspace on 
%%       the screen.  It is a media-level attribute.  Permitted 
%%       values are "portrait", "landscape", and "seascape" 
%%       (upside-down landscape).  It is not dependent on charset.
%%
%%    a=type:<conference type>
%%
%%       This specifies the type of the conference.  Suggested 
%%       values are "broadcast", "meeting", "moderated", "test", and 
%%       "H332". "recvonly" should be the default for 
%%       "type:broadcast" sessions, "type:meeting" should imply 
%%       "sendrecv", and "type:moderated" should indicate the use of 
%%       a floor control tool and that the media tools are started 
%%       so as to mute new sites joining the conference.
%%
%%       Specifying the attribute "type:H332" indicates that this
%%       loosely coupled session is part of an H.332 session as 
%%       defined in the ITU H.332 specification [26].  Media tools 
%%       should be started "recvonly".
%%
%%       Specifying the attribute "type:test" is suggested as a hint
%%       that, unless explicitly requested otherwise, receivers can
%%       safely avoid displaying this session description to users.
%%
%%       The type attribute is a session-level attribute, and it is 
%%       not dependent on charset.
%%
%%    a=charset:<character set>
%%
%%       This specifies the character set to be used to display the
%%       session name and information data.  By default, the 
%%       ISO-10646 character set in UTF-8 encoding is used.  If a 
%%       more compact representation is required, other character 
%%       sets may be used. For example, the ISO 8859-1 is specified 
%%       with the following SDP attribute:
%%
%%          a=charset:ISO-8859-1
%%
%%       This is a session-level attribute and is not dependent on
%%       charset.  The charset specified MUST be one of those 
%%       registered with IANA, such as ISO-8859-1.  The character 
%%       set identifier is a US-ASCII string and MUST be compared 
%%       against the IANA identifiers using a case-insensitive 
%%       comparison.  If the identifier is not recognised or not 
%%       supported, all strings that are affected by it SHOULD be 
%%       regarded as octet strings.
%%
%%       Note that a character set specified MUST still prohibit 
%%       the use of bytes 0x00 (Nul), 0x0A (LF), and 0x0d (CR).  
%%       Character sets requiring the use of these characters MUST 
%%       define a quoting mechanism that prevents these bytes from 
%%       appearing within text fields.
%%
%%    a=sdplang:<language tag>
%%
%%       This can be a session-level attribute or a media-level
%%       attribute.  As a session-level attribute, it specifies the
%%       language for the session description.  As a media-level
%%       attribute, it specifies the language for any media-level 
%%       SDP information field associated with that media.  Multiple 
%%       sdplang attributes can be provided either at session or 
%%       media level if multiple languages in the session description 
%%       or media use multiple languages, in which case the order of 
%%       the attributes indicates the order of importance of the 
%%       various languages in the session or media from most important 
%%       to least important.
%%
%%       In general, sending session descriptions consisting of 
%%       multiple languages is discouraged.  Instead, multiple 
%%       descriptions SHOULD be sent describing the session, one in 
%%       each language. However, this is not possible with all 
%%       transport mechanisms, and so multiple sdplang attributes 
%%       are allowed although NOT RECOMMENDED.
%%
%%       The "sdplang" attribute value must be a single RFC 3066
%%       language tag in US-ASCII [9].  It is not dependent on the
%%       charset attribute.  An "sdplang" attribute SHOULD be 
%%       specified when a session is of sufficient scope to cross 
%%       geographic boundaries where the language of recipients 
%%       cannot be assumed, or where the session is in a different 
%%       language from the locally assumed norm.
%%
%%    a=lang:<language tag>
%%
%%       This can be a session-level attribute or a media-level
%%       attribute.  As a session-level attribute, it specifies the
%%       default language for the session being described.  As a 
%%       media-level attribute, it specifies the language for that 
%%       media, overriding any session-level language specified.  
%%       Multiple lang attributes can be provided either at session 
%%       or media level if the session description or media use 
%%       multiple languages, in which case the order of the 
%%       attributes indicates the order of importance of the various 
%%       languages in the session or media from most important to 
%%       least important.
%%
%%       The "lang" attribute value must be a single RFC 3066 
%%       language tag in US-ASCII [9].  It is not dependent on the 
%%       charset attribute.  A "lang" attribute SHOULD be specified 
%%       when a session is of sufficient scope to cross geographic 
%%       boundaries where the language of recipients cannot be 
%%       assumed, or where the session is in a different language 
%%       from the locally assumed norm.
%%
%%    a=framerate:<frame rate>
%%
%%       This gives the maximum video frame rate in frames/sec.  
%%       It is intended as a recommendation for the encoding of 
%%       video data. Decimal representations of fractional values 
%%       using the notation "<integer>.<fraction>" are allowed.  It 
%%       is a media-level attribute, defined only for video media, 
%%       and it is not dependent on charset.
%%
%%    a=quality:<quality>
%%
%%       This gives a suggestion for the quality of the encoding 
%%       as an integer value.  The intention of the quality 
%%       attribute for video is to specify a non-default trade-off 
%%       between frame-rate and still-image quality.  For video, 
%%       the value is in the range 0 to 10, with the following 
%%       suggested meaning:
%%
%%          10 - the best still-image quality the compression 
%%               scheme can give.
%%          5  - the default behaviour given no quality suggestion.
%%          0  - the worst still-image quality the codec designer 
%%               thinks is still usable.
%%
%%       It is a media-level attribute, and it is not dependent on
%%       charset.
%%
%%    a=fmtp:<format> <format specific parameters>
%%
%%       This attribute allows parameters that are specific to a
%%       particular format to be conveyed in a way that SDP does 
%%       not have to understand them.  The format must be one of 
%%       the formats specified for the media.  Format-specific 
%%       parameters may be any set of parameters required to be 
%%       conveyed by SDP and given unchanged to the media tool that 
%%       will use this format.  At most one instance of this 
%%       attribute is allowed for each format.
%%
%%       It is a media-level attribute, and it is not dependent on
%%       charset.
%% 


%% a=<attribute>
%% a=<attribute>:<value>
-record(megaco_sdp_a, {
	  attribute,                            % string()
	  value                                 % undefined | string()
	 }
       ).

%% a=cat:<category>
-record(megaco_sdp_a_cat, {
	  category                              % string()
	 }
       ).

%% a=keywds:<keywords>
-record(megaco_sdp_a_keywds, {
	  keywords                              % string()
	 }
       ).

%% a=tool:<name and version of tool>
-record(megaco_sdp_a_tool, {
	  name_and_version                      % string()
	 }
       ).

%% a=ptime:<packet time>
-record(megaco_sdp_a_ptime, {
          packet_time                           % integer()
         }
       ).

%% a=maxptime:<maximum packet time>
-record(megaco_sdp_a_maxptime, {
          maximum_packet_time                   % integer()
         }
       ).

%% a=rtpmap:<payload type> <encoding name>/<clock rate> [/<encoding parameters>]
-record(megaco_sdp_a_rtpmap, {
          payload_type,                         % <payload type>  integer()
          encoding_name,                        % <encoding name> string()
          clock_rate,                           % <clock rate>    integer()
          encoding_parms = []                   % <encoding parameters> [ string() ]
         }
       ).
               
%% a=orient:<orientation>
%% orientation() -> portrait | landscape | seascape
-record(megaco_sdp_a_orient, {
           orientation                          % orientation()
         }
       ).


%% a=type:<conference type>
-record(megaco_sdp_a_type, {
           conf_type                            % string()
         }
       ).
 

%% a=charset:<character set>
-record(megaco_sdp_a_charset, {
           char_set                            % string()
         }
       ).


%% a=sdplang:<language tag>
-record(megaco_sdp_a_sdplang, {
           tag                                 % string()
         }
       ).


%% a=lang:<language tag>
-record(megaco_sdp_a_lang, {
           tag                                 % string()
         }
       ).


%% a=framerate:<frame rate>
-record(megaco_sdp_a_framerate, {
           frame_rate                          % string() 
         }
       ).


%% a=quality:<quality>
-record(megaco_sdp_a_quality, {
          quality                              % integer()
         }
       ).


%% a=fmtp:<format> <format specific parameters>
-record(megaco_sdp_a_fmtp, {
          format,                               % string()
          param                                 % string()
         }
       ).




%% ===================================================================
%% 
%% Media Announcements ("m=")
%% 
%% m=<media> <port> <proto> <fmt> ...
%%
%% A session description may contain a number of media descriptions.
%% Each media description starts with an "m=" field and is terminated 
%% by either the next "m=" field or by the end of the session 
%% description. A media field has several sub-fields:
%%
%% <media> is the media type.  Currently defined media are "audio",
%%    "video", "text", "application", and "message", although this 
%%    list may be extended in the future (see Section 8 of RFC 4566).
%%
%% <port> is the transport port to which the media stream is sent.  
%%    The meaning of the transport port depends on the network being 
%%    used as specified in the relevant "c=" field, and on the 
%%    transport protocol defined in the <proto> sub-field of the 
%%    media field. Other ports used by the media application (such as 
%%    the RTP Control Protocol (RTCP) port [19]) MAY be derived 
%%    algorithmically from the base media port or MAY be specified in 
%%    a separate attribute (for example, "a=rtcp:" as defined in 
%%    [22]).
%%
%%    If non-contiguous ports are used or if they don't follow the
%%    parity rule of even RTP ports and odd RTCP ports, the "a=rtcp:"
%%    attribute MUST be used.  Applications that are requested to send
%%    media to a <port> that is odd and where the "a=rtcp:" is present
%%    MUST NOT subtract 1 from the RTP port: that is, they MUST send 
%%    the RTP to the port indicated in <port> and send the RTCP to the 
%%    port indicated in the "a=rtcp" attribute.
%%
%%    For applications where hierarchically encoded streams are being
%%    sent to a unicast address, it may be necessary to specify 
%%    multiple transport ports.  This is done using a similar notation 
%%    to that used for IP multicast addresses in the "c=" field:
%%
%%       m=<media> <port>/<number of ports> <proto> <fmt> ...
%%
%%    In such a case, the ports used depend on the transport protocol.
%%    For RTP, the default is that only the even-numbered ports are 
%%    used for data with the corresponding one-higher odd ports used 
%%    for the RTCP belonging to the RTP session, and the 
%%    <number of ports> denoting the number of RTP sessions.  For 
%%    example:
%%
%%       m=video 49170/2 RTP/AVP 31
%%
%%    would specify that ports 49170 and 49171 form one RTP/RTCP pair
%%    and 49172 and 49173 form the second RTP/RTCP pair.  RTP/AVP is 
%%    the transport protocol and 31 is the format (see below).  If 
%%    non-contiguous ports are required, they must be signalled using 
%%    a separate attribute (for example, "a=rtcp:" as defined in 
%%    [22]).
%%
%%    If multiple addresses are specified in the "c=" field and 
%%    multiple ports are specified in the "m=" field, a one-to-one 
%%    mapping from port to the corresponding address is implied.  For 
%%    example:
%%
%%       c=IN IP4 224.2.1.1/127/2
%%       m=video 49170/2 RTP/AVP 31
%%
%%    would imply that address 224.2.1.1 is used with ports 49170 
%%    and 49171, and address 224.2.1.2 is used with ports 49172 and 
%%    49173.
%%
%%    The semantics of multiple "m=" lines using the same transport
%%    address are undefined.  This implies that, unlike limited past
%%    practice, there is no implicit grouping defined by such means 
%%    and an explicit grouping framework (for example, [18]) should 
%%    instead be used to express the intended semantics.
%%
%% <proto> is the transport protocol.  The meaning of the transport
%%    protocol is dependent on the address type field in the 
%%    relevant "c=" field.  Thus a "c=" field of IP4 indicates that 
%%    the transport protocol runs over IP4.  The following transport 
%%    protocols are defined, but may be extended through 
%%    registration of new protocols with IANA (see Section 8 of RFC 
%%    4566):
%%
%%    *  udp: denotes an unspecified protocol running over UDP.
%%
%%    *  RTP/AVP: denotes RTP [19] used under the RTP Profile for 
%%       Audio and Video Conferences with Minimal Control [20] 
%%       running over UDP.
%%
%%    *  RTP/SAVP: denotes the Secure Real-time Transport Protocol 
%%       [23] running over UDP.
%%
%%    The main reason to specify the transport protocol in addition 
%%    to the media format is that the same standard media formats 
%%    may be carried over different transport protocols even when 
%%    the network protocol is the same -- a historical example is 
%%    vat Pulse Code Modulation (PCM) audio and RTP PCM audio; 
%%    another might be TCP/RTP PCM audio.  In addition, relays and 
%%    monitoring tools that are transport-protocol-specific but 
%%    format-independent are possible.
%%
%% <fmt> is a media format description.  The fourth and any 
%%    subsequent sub-fields describe the format of the media.  The 
%%    interpretation of the media format depends on the value of 
%%    the <proto> sub-field.
%%
%%    If the <proto> sub-field is "RTP/AVP" or "RTP/SAVP" the <fmt>
%%    sub-fields contain RTP payload type numbers.  When a list of
%%    payload type numbers is given, this implies that all of these
%%    payload formats MAY be used in the session, but the first of 
%%    these formats SHOULD be used as the default format for the 
%%    session.  For dynamic payload type assignments the "a=rtpmap:" 
%%    attribute (see Section 6 of RFC 4566) SHOULD be used to map 
%%    from an RTP payload type number to a media encoding name that 
%%    identifies the payload format.  The "a=fmtp:"  attribute MAY 
%%    be used to specify format parameters (see Section 6 of RFC 
%%    4566).
%%
%%    If the <proto> sub-field is "udp" the <fmt> sub-fields MUST
%%    reference a media type describing the format under the 
%%    "audio", "video", "text", "application", or "message" 
%%    top-level media types.  The media type registration SHOULD 
%%    define the packet format for use with UDP transport.
%%
%%    For media using other transport protocols, the <fmt> field is
%%    protocol specific.  Rules for interpretation of the <fmt> sub-
%%    field MUST be defined when registering new protocols (see 
%%    Section 8.2.2 of RFC 4566).
%% 

%% ma_media() -> audio | video | application | data | control
-record(megaco_sdp_m, {
          media,         % ma_media() | string()
          port,          % integer()
          num_ports,     % undefined | integer()
          transport,     % string()
          fmt_list = []  % [ string() ]
         }).




%% ===================================================================
%% 
%% References
%% 
%% Normative References
%%
%% [1]   Mockapetris, P., "Domain names - concepts and facilities", STD
%%       13, RFC 1034, November 1987.
%%
%% [2]   Mockapetris, P., "Domain names - implementation and
%%       specification", STD 13, RFC 1035, November 1987.
%%
%% [3]   Bradner, S., "Key words for use in RFCs to Indicate Requirement
%%       Levels", BCP 14, RFC 2119, March 1997.
%%
%% [4]   Crocker, D., Ed. and P. Overell, "Augmented BNF for Syntax
%%       Specifications: ABNF", RFC 4234, October 2005.
%%
%% [5]   Yergeau, F., "UTF-8, a transformation format of ISO 10646", STD
%%       63, RFC 3629, November 2003.
%%
%% [6]   Handley, M. and V. Jacobson, "SDP: Session Description
%%       Protocol", RFC 2327, April 1998.
%%
%% [7]   Berners-Lee, T., Fielding, R., and L. Masinter, "Uniform
%%       Resource Identifier (URI): Generic Syntax", STD 66, RFC 3986,
%%       January 2005.
%%
%% [8]   Narten, T. and H. Alvestrand, "Guidelines for Writing an IANA
%%       Considerations Section in RFCs", BCP 26, RFC 2434, October
%%       1998.
%%
%% [9]   Alvestrand, H., "Tags for the Identification of Languages", BCP
%%       47, RFC 3066, January 2001.
%%
%% [10]  Olson, S., Camarillo, G., and A. Roach, "Support for IPv6 in
%%       Session Description Protocol (SDP)", RFC 3266, June 2002.
%%
%% [11]  Faltstrom, P., Hoffman, P., and A. Costello,
%%       "Internationalizing Domain Names in Applications (IDNA)", RFC
%%       3490, March 2003.
%%
%% [12]  Josefsson, S., "The Base16, Base32, and Base64 Data Encodings",
%%       RFC 3548, July 2003.
%% 
%% 
%% Informative References
%%
%% [13]  Mills, D., "Network Time Protocol (Version 3) Specification,
%%       Implementation", RFC 1305, March 1992.
%%
%% [14]  Handley, M., Perkins, C., and E. Whelan, "Session Announcement
%%       Protocol", RFC 2974, October 2000.
%%
%% [15]  Rosenberg, J., Schulzrinne, H., Camarillo, G., Johnston, A.,
%%       Peterson, J., Sparks, R., Handley, M., and E. Schooler, "SIP:
%%       Session Initiation Protocol", RFC 3261, June 2002.
%%
%% [16]  Schulzrinne, H., Rao, A., and R. Lanphier, "Real Time Streaming
%%       Protocol (RTSP)", RFC 2326, April 1998.
%%
%% [17]  Rosenberg, J. and H. Schulzrinne, "An Offer/Answer Model with
%%       Session Description Protocol (SDP)", RFC 3264, June 2002.
%%
%% [18]  Camarillo, G., Eriksson, G., Holler, J., and H. Schulzrinne,
%%       "Grouping of Media Lines in the Session Description Protocol
%%       (SDP)", RFC 3388, December 2002.
%%
%% [19]  Schulzrinne, H., Casner, S., Frederick, R., and V. Jacobson,
%%       "RTP: A Transport Protocol for Real-Time Applications", STD 64,
%%       RFC 3550, July 2003.
%%
%% [20]  Schulzrinne, H. and S. Casner, "RTP Profile for Audio and Video
%%       Conferences with Minimal Control", STD 65, RFC 3551, July 2003.
%%
%% [21]  Casner, S., "Session Description Protocol (SDP) Bandwidth
%%       Modifiers for RTP Control Protocol (RTCP) Bandwidth", RFC 3556,
%%       July 2003.
%%
%% [22]  Huitema, C., "Real Time Control Protocol (RTCP) attribute in
%%       Session Description Protocol (SDP)", RFC 3605, October 2003.
%%
%% [23]  Baugher, M., McGrew, D., Naslund, M., Carrara, E., and K.
%%       Norrman, "The Secure Real-time Transport Protocol (SRTP)", RFC
%%       3711, March 2004.
%%
%% [24]  Rosenberg, J., Schulzrinne, H., and P. Kyzivat, "Indicating
%%       User Agent Capabilities in the Session Initiation Protocol
%%       (SIP)", RFC 3840, August 2004.
%%
%% [25]  Westerlund, M., "A Transport Independent Bandwidth Modifier for
%%       the Session Description Protocol (SDP)", RFC 3890, September
%%       2004.
%%
%% [26]  International Telecommunication Union, "H.323 extended for
%%       loosely coupled conferences", ITU Recommendation H.332,
%%       September 1998.
%%
%% [27]  Arkko, J., Carrara, E., Lindholm, F., Naslund, M., and K.
%%       Norrman, "Key Management Extensions for Session Description
%%       Protocol (SDP) and Real Time Streaming Protocol (RTSP)", RFC
%%       4567, July 2006.
%%
%% [28]  Andreasen, F., Baugher, M., and D. Wing, "Session Description
%%       Protocol (SDP) Security Descriptions for Media Streams", RFC
%%       4568, July 2006.
%%
%% [29]  Resnick, P., "Internet Message Format", RFC 2822, April 2001.
%%
%% [30]  Hinden, R. and S. Deering, "IP Version 6 Addressing
%%       Architecture", RFC 2373, July 1998.
%%
%% [31]  Freed, N. and J. Klensin, "Media Type Specifications and
%%       Registration Procedures", BCP 13, RFC 4288, December 2005.
%% 


-endif.
