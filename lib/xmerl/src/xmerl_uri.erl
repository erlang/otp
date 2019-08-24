%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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


-module(xmerl_uri).


-export([parse/1,resolve/2
%	 scan_abspath/1
	]).


%%% Parse URI and return {Scheme,Path}
%%% Note that Scheme specific parsing/validation is not handled here!
resolve(_Root,_Rel) ->
    ok.

%%% See "http://www.isi.edu/in-notes/iana/assignments/url-schemes" for a list of
%%% defined URL schemes and references to its sources.

parse(URI) ->
    case parse_scheme(URI) of
	{http,Cont} -> parse_http(Cont,http);
	{https,Cont} -> parse_http(Cont,https);
	{ftp,Cont} -> parse_ftp(Cont,ftp);
	{sip,Cont} -> parse_sip(Cont,sip);
	{sips,Cont} -> parse_sip(Cont,sips);
	{sms,Cont} -> parse_sms(Cont,sms); % Note: From old draft
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
%%% Returns a tuple {http,Host,Port,Path,Query} where
%%%  Host  = string() Host value
%%%  Port  = string() Port value
%%%  Path  = string() Absolute path
%%%  Query = string() Query or Fragment value
parse_http("//"++C0,Scheme) ->
    case scan_hostport(C0,Scheme) of
	{C1,Host,Port} ->
	    case scan_pathquery(C1) of
		{error,Error} ->
		    {error,Error};
		{Path,Query} ->
		    {Scheme,Host,Port,Path,Query}
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
	    {"/",""};
	{"?"++C1,Path} ->
	    case scan_query(C1,[]) of
		{error,Error} ->
		    {error,Error};
		Query ->
		    {Path,"?"++Query}
	    end;
	{"#"++C1,Path} -> % 'query' and 'fragment' are both defined as '*uric'
	    case scan_query(C1,[]) of
		{error,Error} ->
		    {error,Error};
		Fragment ->
		    {Path,"#"++Fragment}
	    end;	    
	{[],Path} ->
	    {Path,""}
    end.


%%% ............................................................................
-define(FTP_DEFAULT_PORT, 21).

%%% FTP (Source RFC 2396, RFC 1738, RFC 959)
%%% Encoded :, @, or / characters appearing within the username or
%%% password fields (as required by RFC 1738) are not handled. 
%%%
%%% Note: This BNF has been modified to better fit with RFC 2396
%%%  ftp_URL = "ftp:" "//" [ ftp_userinfo "@"] host [ ":" port ] ftp_abs_path
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
	{error, Error} ->
	    {error,Error};
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
	    end
    end.

ftp_userinfo(C0) ->
    ftp_userinfo(C0, []).

ftp_userinfo([], Acc) ->
    {lists:reverse(Acc), {"",""}};
ftp_userinfo(C0=[$/ |_], Acc) ->
    {lists:reverse(Acc)++C0, {"",""}};
ftp_userinfo([$@ |C0], Acc) ->
    {C0, ftp_userinfo_1(lists:reverse(Acc), 0, "", "")};
ftp_userinfo([C |C0], Acc) ->
    ftp_userinfo(C0, [C |Acc]).


ftp_userinfo_1([], 0, Acc, []) ->
    { lists:reverse(Acc), ""};
ftp_userinfo_1([], 1, Acc, User) ->
    {User, lists:reverse(Acc)};
ftp_userinfo_1([$:|_], 0, [], []) ->
    {error,no_user};
ftp_userinfo_1([$:|C0], 0, Acc,[]) ->
    ftp_userinfo_1(C0, 1, [], lists:reverse(Acc));

ftp_userinfo_1([C|C0],Stage, Acc, User) ->
    ftp_userinfo_1(C0,Stage, [C|Acc], User).


%%% .........................................................................
-define(SIP_DEFAULT_PORT, 5060).
-define(SIPTLS_DEFAULT_PORT, 5061).

%%% SIP (Source RFC 2396, RFC 3261)
%%% sip_URL = "sip:" [ sip_userinfo "@" ] host [ ":" port ] 
%%%				sip_uri-parameters [ sip_headers ]
%%% sip_userinfo    = (sip_user | sip_telephone-subscriber) 
%%%                             [ ":" sip_password ]
%%% sip_user	     = *( unreserved | escaped | 
%%%                       "&" | "=" | "+" | "$" | "," |  ";" | "?" | "/")
%%% sip_telephone-subscriber = See RFC2806
%%% sip_password    = *( unreserved | escaped |
%%%                       "&" | "=" | "+" | "$" | "," )
%%% sip_uri-parameters  = *( ";" sip_uri-parameter )
%%% sip_uri-parameter   = sip_transport-param  | sip_user-param |
%%%  			       sip_method-param | sip_ttl-param |
%%%			       sip_maddr-param  | sip_lr-param | sip_other-param
%%% sip_transport-param = "transport=" ( "udp" | "tcp" | "sctp" | "tls" | token)
%%% sip_user-param      = "user=" ( "phone" | "ip" | token)
%%% sip_method-param    = "method=" sip_Method
%%% sip_ttl-param       = "ttl=" sip_ttl
%%% sip_maddr-param     = "maddr=" host
%%% sip_lr-param        = "lr"
%%% sip_other-param     = 1*sip_paramchar [ "=" 1*sip_paramchar ]
%%% sip_Method		 = "INVITE" | "ACK" | "OPTIONS" | "BYE" |
%%%				"CANCEL" | "REGISTER" | token
%%% sip_ttl             = 1*3DIGIT       ; 0 to 255
%%% sip_paramchar         =  sip_param-unreserved | unreserved | escaped
%%% sip_param-unreserved  =  "[" | "]" | "/" | ":" | "&" | "+" | "$"
%%% sip_headers         =  "?" sip_header *( "&" sip_header )
%%% sip_header          =  sip_hname "=" sip_hvalue
%%% sip_hname           =  1*( sip_hnv-unreserved | unreserved | escaped )
%%% sip_hvalue          =  *( sip_hnv-unreserved / unreserved / escaped )
%%% sip_hnv-unreserved  =  "[" | "]" | "/" | "?" | ":" | "+" | "$"

%%% Note:
%%% - FIXME: Headers not parsed
parse_sip(C0,Scheme) ->
    case string:tokens(C0,"@") of
	[Userinfo,Hostport] ->
	    {User,Pass}=sip_userinfo(Userinfo),
	    {C1,Host,Port}=scan_hostport(Hostport,Scheme),
	    {C2,Parameters}=scan_parameters(C1),
	    Headers=scan_headers(C2),
	    {Scheme,User,Pass,Host,Port,Parameters,Headers};
	[Hostport] ->
	    {C1,Host,Port}=scan_hostport(Hostport,Scheme),
	    {C2,Parameters}=scan_parameters(C1),
	    Headers=scan_headers(C2),
	    {Scheme,none,none,Host,Port,Parameters,Headers}
    end.

%%% FIXME! User can be telephone subscriber
sip_userinfo(Userinfo) ->
    case string:tokens(Userinfo,":") of
	[User,Pass] -> {User,Pass};
	[User] ->      {User,none}
    end.

scan_parameters(C1) ->
    ParList=string:tokens(C1,";"),
    scan_parameters2(ParList,[], []).

%% Is Foo the way to go? This code needs further investigation. (As
%% does most of this module.) If we decide to keep it!
scan_parameters2([],Out, Foo) ->
    {lists:reverse(Foo), lists:reverse(Out)};
scan_parameters2(["transport"++Val|Rest],Out, Foo) ->
    scan_parameters2(Rest,[{transport,Val}|Out], Foo);
scan_parameters2(["user"++Val|Rest],Out, Foo) ->
    scan_parameters2(Rest,[{user,Val}|Out], Foo);
scan_parameters2(["method"++Val|Rest],Out, Foo) ->
    scan_parameters2(Rest,[{method,Val}|Out], Foo);
scan_parameters2(["ttl"++Val|Rest],Out, Foo) ->
    scan_parameters2(Rest,[{ttl,Val}|Out], Foo);
scan_parameters2(["maddr"++Val|Rest],Out, Foo) ->
    scan_parameters2(Rest,[{maddr,Val}|Out], Foo);
scan_parameters2(["lr"|Rest],Out, Foo) ->
    scan_parameters2(Rest,[{lr,""}|Out], Foo);
scan_parameters2([Other|Rest],Out, Foo) ->
    scan_parameters2(Rest,[Out], [Other |Foo]).

%%% FIXME!
scan_headers(C2) ->
    C2.

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
    

%%% ==========================================================================
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
	    {C1,Host,?FTP_DEFAULT_PORT};
	{C1,Host} when Scheme==sip ->
	    {C1,Host,?SIP_DEFAULT_PORT}
    end.


%%%      host          = hostname | IPv4address | IPv6reference
%%%      hostname      = *( domainlabel "." ) toplabel [ "." ]
%%%      domainlabel   = alphanum | alphanum *( alphanum | "-" ) alphanum
%%%      toplabel      = alpha | alpha *( alphanum | "-" ) alphanum
%%%      IPv4address   = 1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT
%%%      ipv6reference = "[" IPv6address "]"
%%%      IPv6address = hexpart [ ":" IPv4address ]
%%%      hexpart = hexseq | hexseq "::" [ hexseq ] | "::" [ hexseq ]
%%%      hexseq  = hex4 *( ":" hex4)
%%%      hex4    = 1*4HEXDIG

%%% Note:
%%% Bitfields are set as follows:
%%%  Bit 0 = 0-9
%%%  Bit 1 = a-f
%%%  Bit 2 = g-z
-define(BIT1, 1).
-define(BIT2, 2).
-define(BIT3, 4).

%%% 1 = DIGIT are only digits
%%% 3 = HEX   are DIGITS + a-f 
%%% 6 = ALPHA are HEX    - DIGITS + g-z
-define(DIGIT, 1).
-define(HEX,   3).
-define(ALPHA, 6).


scan_host(C0) ->
    case scan_host2(C0,[],0,[],[]) of
	{C1,IPv4address,[?DIGIT,?DIGIT,?DIGIT,?DIGIT]} ->
	    {C1,lists:reverse(lists:append(IPv4address))};
%% 	{C1,IPv6address,[$[,Hex1,Hex2,Hex3,Hex4,$]]} when Hex1=<?HEX;
%% 							  Hex2=<?HEX;
%% 							  Hex3=<?HEX;
%% 							  Hex4=<?HEX ->
%% 	    {C1,lists:reverse(lists:append(IPv6address))};
	{C1,Hostname,[_A|_HostF]} -> 
	    {C1,lists:reverse(lists:append(Hostname))}
%%	_ ->
%%	    {error,no_host}
    end.
    
scan_host2([H|C0],Acc,CurF,Host,HostF) when $0=<H,H=<$9 ->
    scan_host2(C0,[H|Acc],CurF bor ?BIT1,Host,HostF);
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
%%%                      "$" | "," | "[" | "]"
%%%      unreserved    = alphanum | mark
%%%      mark          = "-" | "_" | "." | "!" | "~" | "*" | "'" |
%%%                      "(" | ")"
%%%      escaped       = "%" hex hex
scan_query([],Acc) ->
    lists:reverse(Acc);
scan_query([$%,H1,H2|C0],Acc) -> % escaped
    scan_query([hex2dec(H1)*16+hex2dec(H2)|C0],Acc);
scan_query([H|C0],Acc) when $a=<H,H=<$z;$A=<H,H=<$Z;$0=<H,H=<$9 -> % alphanum
    scan_query(C0,[H|Acc]);
scan_query([H|C0],Acc) when H==$;; H==$/; H==$?; H==$:; H==$@; H==$[; H==$];
			    H==$&; H==$=; H==$+; H==$$; H==$, -> % reserved
    scan_query(C0,[H|Acc]);
scan_query([H|C0],Acc) when H==$-; H==$_; H==$.; H==$!; H==$~;
			    H==$*; H==$'; H==$(; H==$) -> % mark
    scan_query(C0,[H|Acc]);
scan_query([H|C0],Acc) when 0=<H,H=<127 -> % US ASCII
    {H1,H2}=dec2hex(H),
    scan_query(C0,[H2,H1,$%|Acc]);    
scan_query([_H|_C0],_Acc) ->
    {error,no_query}.


%%%      pchar         = unreserved | escaped |
%%%                      ":" | "@" | "&" | "=" | "+" | "$" | ","
scan_pchars([],Acc) ->
    {[],Acc};
scan_pchars([$%,H1,H2|C0],Acc) -> % escaped
    scan_pchars([hex2dec(H1)*16+hex2dec(H2)|C0],Acc);
scan_pchars([H|C0],Acc) when $a=<H,H=<$z;$A=<H,H=<$Z;$0=<H,H=<$9  -> % alphanum
    scan_pchars(C0,[H|Acc]);
scan_pchars([H|C0],Acc) when H==$-; H==$_; H==$.; H==$!; H==$~;
			     H==$*; H==$'; H==$(; H==$) -> % mark
    scan_pchars(C0,[H|Acc]);
scan_pchars([H|C0],Acc) when H==$:; H==$@; H==$&; H==$=; H==$+; H==$$; H==$, ->
    scan_pchars(C0,[H|Acc]);
scan_pchars([H|C0],Acc) when 0=<H,H=<127, % US ASCII
			     H=/=$?,H=/=$;,H=/=$/,H=/=$# -> 
    {H1,H2}=dec2hex(H),
    scan_pchars(C0,[H2,H1,$%|Acc]);    
scan_pchars(C0,Acc) ->
    {C0,Acc}.

hex2dec(X) when X>=$0,X=<$9 -> X-$0;
hex2dec(X) when X>=$A,X=<$F -> X-$A+10;
hex2dec(X) when X>=$a,X=<$f -> X-$a+10.

dec2hex(H) when H<256 ->
    <<H1:4,H2:4>> = <<H>>,
    {nibble2hex(H1),nibble2hex(H2)}.

nibble2hex(X) when 0=<X,X=<9 -> X+$0;
nibble2hex(10) -> $a;
nibble2hex(11) -> $b;
nibble2hex(12) -> $c;
nibble2hex(13) -> $d;
nibble2hex(14) -> $e;
nibble2hex(15) -> $f.
