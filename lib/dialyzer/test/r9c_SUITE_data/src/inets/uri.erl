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
%% The Initial Developer of the Original Code is Mobile Arts AB
%% Portions created by Mobile Arts are Copyright 2002, Mobile Arts AB
%% All Rights Reserved.''
%%
%%
%% Author  : Johan Blom <johblo@localhost.localdomain>
%% Description :
%%   Implements various scheme dependent subsets (e.g. HTTP, FTP etc) based on
%%   RFC 2396, Uniform Resource Identifiers (URI): Generic Syntax
%% Created : 27 Jul 2001 by Johan Blom <johblo@localhost.localdomain>

-module(uri).

-author('johan.blom@mobilearts.se').

-export([parse/1,resolve/2]).


%%% Parse URI and return {Scheme,Path}
%%% Note that Scheme specific parsing/validation is not handled here!
resolve(Root,Rel) ->
    ok.

%%% See "http://www.isi.edu/in-notes/iana/assignments/url-schemes" for a list of
%%% defined URL schemes and references to its sources.

parse(URI) ->
    case parse_scheme(URI) of
	{http,Cont} -> parse_http(Cont,http);
	{https,Cont} -> parse_http(Cont,https);
	{ftp,Cont} -> parse_ftp(Cont,ftp);
	{sip,Cont} -> parse_sip(Cont,sip);
	{sms,Cont} -> parse_sms(Cont,sip);
	{error,Error} -> {error,Error};
	{Scheme,Cont} -> {Scheme,Cont}
    end.


%%% Parse the scheme.
parse_scheme(URI) ->
    parse_scheme(URI,[]).

parse_scheme([H|URI],Acc) when $a=<H,H=<$z; $A=<H,H=<$Z  ->
    parse_scheme2(URI,[H|Acc]);
parse_scheme(_,_) ->
    {error,no_scheme}.

parse_scheme2([H|URI],Acc)
  when $a=<H,H=<$z; $A=<H,H=<$Z; $0=<H,H=<$9; H==$-;H==$+;H==$. ->
    parse_scheme2(URI,[H|Acc]);
parse_scheme2([$:|URI],Acc) ->
    {list_to_atom(lists:reverse(Acc)),URI};
parse_scheme2(_,_) ->
    {error,no_scheme}.


%%% ............................................................................
-define(HTTP_DEFAULT_PORT, 80).
-define(HTTPS_DEFAULT_PORT, 443).

%%% HTTP (Source RFC 2396, RFC 2616)
%%%  http_URL = "*" | absoluteURI | abs_path [ "?" query ] | authority

%%%  http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
%%% Returns a tuple {http,Host,Port,PathQuery} where
%%%  Host     = string() Host value
%%%  Port     = string() Port value
%%%  PathQuery= string() Combined absolute path and query value
parse_http("//"++C0,Scheme) ->
    case scan_hostport(C0,Scheme) of
	{C1,Host,Port} ->
	    case scan_pathquery(C1) of
		{error,Error} ->
		    {error,Error};
		PathQuery ->
		    {Scheme,Host,Port,PathQuery}
	    end;
	{error,Error} ->
	    {error,Error}
    end;
parse_http(_,_) ->
    {error,invalid_url}.

scan_pathquery(C0) ->
    case scan_abspath(C0) of
	{error,Error} ->
	    {error,Error};
	{[],[]} -> % Add implicit path
	    "/";
	{"?"++C1,Path} ->
	    case scan_query(C1,[]) of
		{error,Error} ->
		    {error,Error};
		Query ->
		    Path++"?"++Query
	    end;
	{[],Path} ->
	    Path
    end.


%%% ............................................................................
%%% FIXME!!! This is just a quick hack that doesn't work!
-define(FTP_DEFAULT_PORT, 80).

%%% FTP (Source RFC 2396, RFC 1738, RFC 959)
%%% Note: This BNF has been modified to better fit with RFC 2396
%%%  ftp_URL = "ftp:" "//" [ ftp_userinfo ] host [ ":" port ] ftp_abs_path
%%%  ftp_userinfo	= ftp_user [ ":" ftp_password ]
%%%  ftp_abs_path 	= "/" ftp_path_segments [ ";type=" ftp_type ]
%%%  ftp_path_segments	= ftp_segment *( "/" ftp_segment)
%%%  ftp_segment	= *[ ftp_uchar | "?" | ":" | "@" | "&" | "=" ]
%%%  ftp_type		= "A" | "I" | "D" | "a" | "i" | "d"
%%%  ftp_user		= *[ ftp_uchar | ";" | "?" | "&" | "=" ]
%%%  ftp_password	= *[ ftp_uchar | ";" | "?" | "&" | "=" ]
%%%  ftp_uchar		= ftp_unreserved | escaped
%%%  ftp_unreserved	= alphanum | mark | "$" | "+" | ","
parse_ftp("//"++C0,Scheme) ->
    case ftp_userinfo(C0) of
	{C1,Creds} ->
	    case scan_hostport(C1,Scheme) of
		{C2,Host,Port} ->
		    case scan_abspath(C2) of
			{error,Error} ->
			    {error,Error};
			{[],[]} -> % Add implicit path
			    {Scheme,Creds,Host,Port,"/"};
			{[],Path} ->
			    {Scheme,Creds,Host,Port,Path}
		    end;
		{error,Error} ->
		    {error,Error}
	    end;
	{error,Error} ->
	    {error,Error}
    end.

ftp_userinfo(C0) ->
    User="",
    Password="",
    {C0,{User,Password}}.


%%% ............................................................................
%%% SIP (Source RFC 2396, RFC 2543)
%%%  sip_URL = "sip:" [ sip_userinfo "@" ] host [ ":" port ]
%%%				sip_url-parameters [ sip_headers ]
%%%  sip_userinfo    = sip_user [ ":" sip_password ]
%%%  sip_user	     = *( unreserved | escaped | "&" | "=" | "+" | "$" | "," )
%%%  sip_password    = *( unreserved | escaped | "&" | "=" | "+" | "$" | "," )
%%%  sip_url-parameters  = *( ";" sip_url-parameter )
%%%  sip_url-parameter   = sip_transport-param | sip_user-param |
%%%  				sip_method-param | sip_ttl-param |
%%%				sip_maddr-param | sip_other-param
%%%  sip_transport-param = "transport=" ( "udp" | "tcp" )
%%%  sip_ttl-param       = "ttl=" sip_ttl
%%%  sip_ttl             = 1*3DIGIT       ; 0 to 255
%%%  sip_maddr-param     = "maddr=" host
%%%  sip_user-param      = "user=" ( "phone" | "ip" )
%%%  sip_method-param    = "method=" sip_Method
%%%  sip_tag-param       = "tag=" sip_UUID
%%%  sip_UUID            = 1*( hex | "-" )
%%%  sip_other-param     = ( token | ( token "=" ( token | quoted-string )))
%%%  sip_Method		 = "INVITE" | "ACK" | "OPTIONS" | "BYE" |
%%%				"CANCEL" | "REGISTER"
%%%  sip_token		 = 1*< any CHAR  except CTL's  or separators>
%%%  sip_quoted-string	 = ( <"> *(qdtext | quoted-pair ) <"> )
%%%  sip_qdtext		 = <any TEXT-UTF8 except <">>
%%%  sip_quoted-pair	 =  " \ " CHAR
parse_sip(Cont,Scheme) ->
    {Scheme,Cont}.




%%% ............................................................................
%%% SMS (Source draft-wilde-sms-uri-01, January 24 2002 and
%%%        draft-allocchio-gstn-01, November 2001)
%%% The syntax definition for "gstn-phone" is taken from
%%% [draft-allocchio-gstn-01], allowing global as well as local telephone
%%% numbers.
%%% Note: This BNF has been modified to better fit with RFC 2396
%%%   sms_URI               =  sms ":" 1*( sms-recipient ) [ sms-body ]
%%%   sms-recipient         =  gstn-phone sms-qualifier
%%%                            [ "," sms-recipient ]
%%%   sms-qualifier         =  *( smsc-qualifier / pid-qualifier )
%%%   smsc-qualifier        =  ";smsc=" SMSC-sub-addr
%%%   pid-qualifier         =  ";pid=" PID-sub-addr
%%%   sms-body              =  ";body=" *urlc
%%%   gstn-phone = ( global-phone / local-phone )
%%%   global-phone = "+" 1*( DIGIT / written-sep )
%%%   local-phone =  [ exit-code ] dial-number / exit-code [ dial-number ]
%%%   exit-code = phone-string
%%%   dial-number = phone-string
%%%   subaddr-string = phone-string
%%%   post-dial = phone-string
%%%   phone-string = 1*( DTMF / pause / tonewait / written-sep )
%%%   DTMF = ( DIGIT / "#" / "*" / "A" / "B" / "C" / "D" )
%%%   written-sep = ( "-" / "." )
%%%   pause = "p"
%%%   tonewait = "w"
parse_sms(Cont,Scheme) ->
    {Scheme,Cont}.


%%% ============================================================================
%%% Generic URI parsing. BNF rules from RFC 2396

%%%      hostport      = host [ ":" port ]
scan_hostport(C0,Scheme) ->
    case scan_host(C0) of
	{error,Error} ->
	    {error,Error};
	{":"++C1,Host} ->
	    {C2,Port}=scan_port(C1,[]),
	    {C2,Host,list_to_integer(Port)};
	{C1,Host} when Scheme==http ->
	    {C1,Host,?HTTP_DEFAULT_PORT};
	{C1,Host} when Scheme==https ->
	    {C1,Host,?HTTPS_DEFAULT_PORT};
	{C1,Host} when Scheme==ftp ->
	    {C1,Host,?FTP_DEFAULT_PORT}
    end.


%%%      host          = hostname | IPv4address
%%%      hostname      = *( domainlabel "." ) toplabel [ "." ]
%%%      domainlabel   = alphanum | alphanum *( alphanum | "-" ) alphanum
%%%      toplabel      = alpha | alpha *( alphanum | "-" ) alphanum
%%%      IPv4address   = 1*digit "." 1*digit "." 1*digit "." 1*digit

-define(ALPHA, 1).
-define(DIGIT, 2).

scan_host(C0) ->
    case scan_host2(C0,[],0,[],[]) of
	{C1,IPv4address,[?DIGIT,?DIGIT,?DIGIT,?DIGIT]} ->
	    {C1,lists:reverse(lists:append(IPv4address))};
	{C1,Hostname,[?ALPHA|HostF]} ->
	    {C1,lists:reverse(lists:append(Hostname))};
	_ ->
	    {error,no_host}
    end.

scan_host2([H|C0],Acc,CurF,Host,HostF) when $0=<H,H=<$9 ->
    scan_host2(C0,[H|Acc],CurF bor ?DIGIT,Host,HostF);
scan_host2([H|C0],Acc,CurF,Host,HostF) when $a=<H,H=<$z; $A=<H,H=<$Z ->
    scan_host2(C0,[H|Acc],CurF bor ?ALPHA,Host,HostF);
scan_host2([$-|C0],Acc,CurF,Host,HostF) when CurF=/=0 ->
    scan_host2(C0,[$-|Acc],CurF,Host,HostF);
scan_host2([$.|C0],Acc,CurF,Host,HostF) when CurF=/=0 ->
    scan_host2(C0,[],0,[".",Acc|Host],[CurF|HostF]);
scan_host2(C0,Acc,CurF,Host,HostF) ->
    {C0,[Acc|Host],[CurF|HostF]}.


%%%      port          = *digit
scan_port([H|C0],Acc) when $0=<H,H=<$9 ->
    scan_port(C0,[H|Acc]);
scan_port(C0,Acc) ->
    {C0,lists:reverse(Acc)}.

%%%      abs_path      = "/"  path_segments
scan_abspath([]) ->
    {[],[]};
scan_abspath("/"++C0) ->
    scan_pathsegments(C0,["/"]);
scan_abspath(_) ->
    {error,no_abspath}.

%%%      path_segments = segment *( "/" segment )
scan_pathsegments(C0,Acc) ->
    case scan_segment(C0,[]) of
	{"/"++C1,Segment} ->
	    scan_pathsegments(C1,["/",Segment|Acc]);
	{C1,Segment} ->
	    {C1,lists:reverse(lists:append([Segment|Acc]))}
    end.


%%%      segment       = *pchar *( ";" param )
%%%      param         = *pchar
scan_segment(";"++C0,Acc) ->
    {C1,ParamAcc}=scan_pchars(C0,";"++Acc),
    scan_segment(C1,ParamAcc);
scan_segment(C0,Acc) ->
    case scan_pchars(C0,Acc) of
	{";"++C1,Segment} ->
	    {C2,ParamAcc}=scan_pchars(C1,";"++Segment),
	    scan_segment(C2,ParamAcc);
	{C1,Segment} ->
	    {C1,Segment}
    end.

%%%      query         = *uric
%%%      uric          = reserved | unreserved | escaped
%%%      reserved      = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" |
%%%                      "$" | ","
%%%      unreserved    = alphanum | mark
%%%      mark          = "-" | "_" | "." | "!" | "~" | "*" | "'" |
%%%                      "(" | ")"
%%%      escaped       = "%" hex hex
scan_query([],Acc) ->
    lists:reverse(Acc);
scan_query([$%,H1,H2|C0],Acc) -> % escaped
    scan_query(C0,[hex2dec(H1)*16+hex2dec(H2)|Acc]);
scan_query([H|C0],Acc) when $a=<H,H=<$z;$A=<H,H=<$Z;$0=<H,H=<$9 -> % alphanum
    scan_query(C0,[H|Acc]);
scan_query([H|C0],Acc) when H==$;; H==$/; H==$?; H==$:; H==$@;
			    H==$&; H==$=; H==$+; H==$$; H==$, -> % reserved
    scan_query(C0,[H|Acc]);
scan_query([H|C0],Acc) when H==$-; H==$_; H==$.; H==$!; H==$~;
			    H==$*; H==$'; H==$(; H==$) -> % mark
    scan_query(C0,[H|Acc]);
scan_query([H|C0],Acc) ->
    {error,no_query}.


%%%      pchar         = unreserved | escaped |
%%%                      ":" | "@" | "&" | "=" | "+" | "$" | ","
scan_pchars([],Acc) ->
    {[],Acc};
scan_pchars([$%,H1,H2|C0],Acc) -> % escaped
    scan_pchars(C0,[hex2dec(H1)*16+hex2dec(H2)|Acc]);
scan_pchars([H|C0],Acc) when $a=<H,H=<$z;$A=<H,H=<$Z;$0=<H,H=<$9  -> % alphanum
    scan_pchars(C0,[H|Acc]);
scan_pchars([H|C0],Acc) when H==$-; H==$_; H==$.; H==$!; H==$~;
			     H==$*; H==$'; H==$(; H==$) -> % mark
    scan_pchars(C0,[H|Acc]);
scan_pchars([H|C0],Acc) when H==$:; H==$@; H==$&; H==$=; H==$+; H==$$; H==$, ->
    scan_pchars(C0,[H|Acc]);
scan_pchars(C0,Acc) ->
    {C0,Acc}.

hex2dec(X) when X>=$0,X=<$9 -> X-$0;
hex2dec(X) when X>=$A,X=<$F -> X-$A+10;
hex2dec(X) when X>=$a,X=<$f -> X-$a+10.
