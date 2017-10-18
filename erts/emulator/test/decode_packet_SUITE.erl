%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% Test suite for erlang:decode_packet/3

-module(decode_packet_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,
         init_per_testcase/2,end_per_testcase/2,
         basic/1, packet_size/1, neg/1, http/1, line/1, ssl/1, otp_8536/1,
         otp_9389/1, otp_9389_line/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [basic, packet_size, neg, http, line, ssl, otp_8536,
     otp_9389, otp_9389_line].

groups() -> 
    [].

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    rand:seed(exsplus),
    io:format("*** SEED: ~p ***\n", [rand:export_seed()]),
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

basic(Config) when is_list(Config) ->
    Packet = <<101,22,203,54,175>>,
    Rest = <<123,34,0,250>>,
    Bin = <<Packet/binary,Rest/binary>>,
    {ok, Bin, <<>>} = decode_pkt(raw,Bin),

    {more, 5+1} = decode_pkt(1,<<5,1,2,3,4>>),
    {more, 5+2} = decode_pkt(2,<<0,5,1,2,3,4>>),
    {more, 5+4} = decode_pkt(4,<<0,0,0,5,1,2,3,4>>),

    {more, undefined} = decode_pkt(1,<<>>),
    {more, undefined} = decode_pkt(2,<<0>>),
    {more, undefined} = decode_pkt(4,<<0,0,0>>),

    Types = [1,2,4,asn1,sunrm,cdr,fcgi,tpkt,ssl_tls],

    %% Run tests for different header types and bit offsets.

    lists:foreach(fun({Type,Bits})->basic_pack(Type,Packet,Rest,Bits), 
                                    more_length(Type,Packet,Bits) end,
                  [{T,B} || T<-Types, B<-lists:seq(0,32)]),
    ok.

basic_pack(Type,Body,Rest,BitOffs) ->
    {Bin,Unpacked,_} = pack(Type,Body,Rest,BitOffs),
    {ok, Unpacked, Rest} = decode_pkt(Type,Bin),
    case Rest of
        <<>> -> ok;
        _ -> 
            <<_:1,NRest/bits>> = Rest,
            basic_pack(Type,Body,NRest,BitOffs)
    end.

more_length(Type,Body,BitOffs) ->
    {Bin,_,_} = pack(Type,Body,<<>>,BitOffs),
    HdrSize = byte_size(Bin) - byte_size(Body),
    more_length_do(Type,HdrSize,Bin,byte_size(Bin)).

more_length_do(_,_,_,0) ->
    ok;
more_length_do(Type,HdrSize,Bin,Size) ->
    TrySize = (Size*3) div 4,
    NSize = if TrySize < HdrSize -> Size - 1;
               true -> TrySize
            end,
    {B1,_} = split_binary(Bin,NSize),
    {more, Length} = decode_pkt(Type,B1),
    case Length of
        L when L=:=byte_size(Bin) -> ok;
        undefined when NSize<HdrSize -> ok
    end,
    more_length_do(Type,HdrSize,Bin,NSize).



pack(Type,Packet,Rest) ->
    {Bin,Unpacked} = pack(Type,Packet),
    {<<Bin/binary,Rest/bits>>,Unpacked}.

%pack(0,B,R,Bits) -> 
%    pack(raw,B,R,Bits);
%pack(raw,Body,Rest,BitOffs) ->
%    Orig = <<0:BitOffs,Body/binary,Rest/bits>>,
%    <<_:BitOffs,Bin/bits>> = Orig,
%    {Bin,<<Bin/binary,Rest/bits>>,Orig};
pack(Type,Body,Rest,BitOffs) ->
    {Packet,Unpacked} = pack(Type,Body),

    %% Make Bin a sub-bin with an arbitrary bitoffset within Orig
    Prefix = rand:uniform(1 bsl BitOffs) - 1,
    Orig = <<Prefix:BitOffs,Packet/binary,Rest/bits>>,
    <<_:BitOffs,Bin/bits>> = Orig,
    {Bin,Unpacked,Orig}.

pack(1,Bin) ->
    Psz = byte_size(Bin),
    {<<Psz:8,Bin/binary>>, Bin};
pack(2,Bin) ->
    Psz = byte_size(Bin),
    {<<Psz:16,Bin/binary>>, Bin};
pack(4,Bin) ->
    Psz = byte_size(Bin),
    {<<Psz:32,Bin/binary>>, Bin};
pack(asn1,Bin) ->
    Ident = case rand:uniform(3) of
                1 -> <<17>>;
                2 -> <<16#1f,16#81,17>>;
                3 -> <<16#1f,16#81,16#80,16#80,17>>
            end,
    Psz = byte_size(Bin),
    Length = case rand:uniform(4) of
                 1 when Psz < 128 -> 
                     <<Psz:8>>;
                 R when R=<2 andalso Psz < 16#10000 ->
                     <<16#82,Psz:16>>;
                 R when R=<3 andalso Psz < 16#1000000 -> 
                     <<16#83,Psz:24>>;
                 _ when Psz < 16#100000000 ->
                     <<16#84,Psz:32>>
             end,
    Res = <<Ident/binary,Length/binary,Bin/binary>>,
    {Res,Res};
pack(sunrm,Bin) ->
    Psz = byte_size(Bin),
    Res = if Psz < 16#80000000 ->
                 <<Psz:32,Bin/binary>>
          end,
    {Res,Res};
pack(cdr,Bin) ->
    GIOP = <<"GIOP">>,
    Major = rand:uniform(256) - 1,
    Minor = rand:uniform(256) - 1,
    MType = rand:uniform(256) - 1,
    Psz = byte_size(Bin),
    Res = case rand:uniform(2) of
              1 -> <<GIOP/binary,Major:8,Minor:8,0:8,MType:8,Psz:32/big,Bin/binary>>;
              2 -> <<GIOP/binary,Major:8,Minor:8,1:8,MType:8,Psz:32/little,Bin/binary>>
          end,
    {Res,Res};
pack(fcgi,Bin) ->
    Ver = 1,
    Type = rand:uniform(256) - 1,
    Id = rand:uniform(65536) - 1,
    PaddSz = rand:uniform(16) - 1,
    Psz = byte_size(Bin),
    Reserv = rand:uniform(256) - 1,
    Padd = case PaddSz of
               0 -> <<>>;
               _ -> list_to_binary([rand:uniform(256)-1
                                    || _<- lists:seq(1,PaddSz)])
           end,
    Res = <<Ver:8,Type:8,Id:16,Psz:16/big,PaddSz:8,Reserv:8,Bin/binary>>,
    {<<Res/binary,Padd/binary>>, Res};
pack(tpkt,Bin) ->
    Ver = 3,
    Reserv = rand:uniform(256) - 1,
    Size = byte_size(Bin) + 4,
    Res = <<Ver:8,Reserv:8,Size:16,Bin/binary>>,
    {Res, Res};
pack(ssl_tls,Bin) ->
    Content = case (rand:uniform(256) - 1) of
                  C when C<128 -> C;
                  _ -> v2hello
              end,
    Major = rand:uniform(256) - 1,
    Minor = rand:uniform(256) - 1,
    pack_ssl(Content,Major,Minor,Bin).

pack_ssl(Content, Major, Minor, Body) ->
    case Content of
        v2hello ->
            Size = byte_size(Body),
            Res = <<1:1,(Size+3):15, 1:8, Major:8, Minor:8, Body/binary>>,
            C = 22,
            Data = <<1:8, (Size+2):24, Major:8, Minor:8, Body/binary>>;
        C when is_integer(C) ->
            Size = byte_size(Body),
            Res = <<Content:8, Major:8, Minor:8, Size:16, Body/binary>>,
            Data = Body			    
    end,
    {Res, {ssl_tls,[],C,{Major,Minor}, Data}}.


packet_size(Config) when is_list(Config) ->
    Packet = <<101,22,203,54,175>>,
    Rest = <<123,34,0,250>>,

    F = fun({Type,Max})->
                {Bin,Unpacked} = pack(Type,Packet,Rest),
                case decode_pkt(Type,Bin,[{packet_size,Max}]) of
                    {ok,Unpacked,Rest} when Max=:=0; Max>=byte_size(Packet) ->
                        ok;
                    {error,_} when Max<byte_size(Packet), Max=/=0 ->
                        ok;
                    {error,_} when Type=:=fcgi, Max=/=0 ->
                        %% packet includes random amount of padding
                        ok
                end
        end,
    lists:foreach(F, [{T,D} || T<-[1,2,4,asn1,sunrm,cdr,fcgi,tpkt,ssl_tls],
                               D<-lists:seq(0, byte_size(Packet)*2)]),

    %% Test OTP-8102, "negative" 4-byte sizes.
    lists:foreach(fun(Size) -> 
                          {error,_} = decode_pkt(4,<<Size:32,Packet/binary>>)
                  end, 
                  lists:seq(-10,-1)),

    %% Test OTP-9389, long HTTP header lines.
    Opts = [{packet_size, 128}],
    Pkt = list_to_binary(["GET / HTTP/1.1\r\nHost: localhost\r\nLink: /",
                          lists:duplicate(64, $Y), "\r\n\r\n"]),
    <<Pkt1:50/binary, Pkt2/binary>> = Pkt,
    {ok, {http_request,'GET',{abs_path,"/"},{1,1}}, Rest1} =
    erlang:decode_packet(http, Pkt1, Opts),
    {ok, {http_header,_,'Host',_,"localhost"}, Rest2} =
    erlang:decode_packet(httph, Rest1, Opts),
    {more, undefined} = erlang:decode_packet(httph, Rest2, Opts),
    {ok, {http_header,_,"Link",_,_}, _} =
    erlang:decode_packet(httph, list_to_binary([Rest2, Pkt2]), Opts),

    Pkt3 = list_to_binary(["GET / HTTP/1.1\r\nHost: localhost\r\nLink: /",
                           lists:duplicate(129, $Y), "\r\n\r\n"]),
    {ok, {http_request,'GET',{abs_path,"/"},{1,1}}, Rest3} =
    erlang:decode_packet(http, Pkt3, Opts),
    {ok, {http_header,_,'Host',_,"localhost"}, Rest4} =
    erlang:decode_packet(httph, Rest3, Opts),
    {error, invalid} = erlang:decode_packet(httph, Rest4, Opts),

    ok.


neg(Config) when is_list(Config) ->
    Bin = <<"dummy">>,
    Fun = fun()->dummy end,

    BadargF = fun(T,B,Opts)-> {'EXIT',{badarg,_}} = (catch decode_pkt(T,B,Opts)) end,

    %% Invalid Type args
    lists:foreach(fun(T)-> BadargF(T,Bin,[]) end, 
                  [3,-1,5,2.0,{2},unknown,[],"line",Bin,Fun,self()]),

    %% Invalid Bin args
    lists:foreach(fun(B)-> BadargF(0,B,[]) end, 
                  [3,2.0,unknown,[],"Bin",[Bin],{Bin},Fun,self()]),

    %% Invalid options
    InvOpts = [2,false,self(),Bin,"Options",Fun,
               packet_size,{packet_size},{packet_size,0,false},
               {packet_size,-1},{packet_size,100.0},{packet_size,false},
               {line_length,-1},{line_length,100.0},{line_length,false}],

    lists:foreach(fun(Opt)-> BadargF(0,Bin,Opt),
                             BadargF(0,Bin,[Opt]),
                             BadargF(0,Bin,[Opt,{packet_size,1000}]),
                             BadargF(0,Bin,[{packet_size,1000},Opt]) end,
                  InvOpts),
    ok.


http(Config) when is_list(Config) ->
    <<"foo">> = http_do(http_request("foo")),
    <<" bar">> = http_do(http_request(" bar")),
    <<"Hello!">> = http_do(http_response("Hello!")),

    %% Test all known header atoms
    Val = "dummy value",
    ValB = list_to_binary(Val),
    Rest = <<"Rest">>,    
    HdrF = fun(Str,N) ->  
                   StrA = list_to_atom(Str),
                   StrB = list_to_binary(Str),
                   Bin = <<StrB/binary,": ",ValB/binary,"\r\n",Rest/binary>>,
                   {ok, {http_header,N,StrA,undefined,Val}, Rest} = decode_pkt(httph,Bin),
                   {ok, {http_header,N,StrA,undefined,ValB}, Rest} = decode_pkt(httph_bin,Bin),
                   N + 1
           end,
    lists:foldl(HdrF, 1, http_hdr_strings()),

    %% Test all known method atoms
    MethF = fun(Meth) ->  
                    MethA = list_to_atom(Meth),
                    MethB = list_to_binary(Meth),
                    Bin = <<MethB/binary," /invalid/url HTTP/1.0\r\n",Rest/binary>>,
                    {ok, {http_request,MethA,{abs_path,"/invalid/url"},{1,0}},
                     Rest} = decode_pkt(http,Bin),
                    {ok, {http_request,MethA,{abs_path,<<"/invalid/url">>},{1,0}},
                     Rest} = decode_pkt(http_bin,Bin)
            end,
    lists:foreach(MethF, http_meth_strings()),

    %% Test all uri variants    
    UriF = fun({Str,ResL,ResB}) -> 
                   Bin = <<"GET ",(list_to_binary(Str))/binary," HTTP/1.1\r\n",Rest/binary>>,
                   {ok, {http_request, 'GET', ResL, {1,1}}, Rest} = decode_pkt(http,Bin),
                   {ok, {http_request, 'GET', ResB, {1,1}}, Rest} = decode_pkt(http_bin,Bin) 
           end,
    lists:foreach(UriF, http_uri_variants()),

    %% Response with empty phrase
    {ok,{http_response,{1,1},200,[]},<<>>} = decode_pkt(http, <<"HTTP/1.1 200\r\n">>, []),
    {ok,{http_response,{1,1},200,<<>>},<<>>} = decode_pkt(http_bin, <<"HTTP/1.1 200\r\n">>, []),
    ok.

http_with_bin(http) ->
    http_bin;
http_with_bin(httph) ->
    httph_bin.

http_do(Tup) ->
    http_do(Tup,http).
http_do({Bin, []}, _) ->
    Bin;
http_do({Bin,[{_Line,PL,PB}|Tail]}, Type) ->
    {ok, PL, Rest} = decode_pkt(Type,Bin),
    {ok, PB, Rest} = decode_pkt(http_with_bin(Type),Bin),

    %% Same tests again but as SubBin
    PreLen = rand:uniform(64),
    Prefix = rand:uniform(1 bsl PreLen) - 1,
    SufLen = rand:uniform(64),
    Suffix = rand:uniform(1 bsl SufLen) - 1,
    Orig = <<Prefix:PreLen, Bin/bits, Suffix:SufLen>>,
    BinLen = bit_size(Bin),
    <<_:PreLen, SubBin:BinLen/bits, _/bits>> = Orig, % Make SubBin
    SubBin = Bin, % just to make sure

    {ok, PL, Rest} = decode_pkt(Type,SubBin),
    {ok, PB, Rest} = decode_pkt(http_with_bin(Type),SubBin),
    http_do({Rest, Tail}, httph).

http_request(Msg) ->
    QnA = [{"POST /invalid/url HTTP/1.1\r\n",
            {http_request, 'POST', {abs_path,  "/invalid/url"  }, {1,1}},
            {http_request, 'POST', {abs_path,<<"/invalid/url">>}, {1,1}}},
           {"Connection: close\r\n",
            {http_header,2,'Connection',undefined,  "close"},
            {http_header,2,'Connection',undefined,<<"close">>}},	 
           {"Host\t : localhost:8000\r\n", % white space before :
            {http_header,14,'Host',undefined,  "localhost:8000"},
            {http_header,14,'Host',undefined,<<"localhost:8000">>}},
           {"User-Agent: perl post\r\n",
            {http_header,24,'User-Agent',undefined,  "perl post"},
            {http_header,24,'User-Agent',undefined,<<"perl post">>}},
           {"Content-Length: 4\r\n",
            {http_header,38,'Content-Length',undefined,  "4"},
            {http_header,38,'Content-Length',undefined,<<"4">>}},
           {"Content-Type: text/xml; charset=utf-8\r\n",
            {http_header,42,'Content-Type',undefined,  "text/xml; charset=utf-8"},
            {http_header,42,'Content-Type',undefined,<<"text/xml; charset=utf-8">>}},
           {"Other-Field: with some text\r\n",
            {http_header,0,  "Other-Field"  ,undefined,  "with some text"},
            {http_header,0,<<"Other-Field">>,undefined,<<"with some text">>}},
           {"Make-sure-a-LONG-HEaDer-fIeLd-is-fORMATTED-NicelY: with some text\r\n",
            {http_header,0,  "Make-Sure-A-Long-Header-Field-Is-Formatted-Nicely"  ,undefined,  "with some text"},
            {http_header,0,<<"Make-Sure-A-Long-Header-Field-Is-Formatted-Nicely">>,undefined,<<"with some text">>}},
           {"Multi-Line: Once upon a time in a land far far away,\r\n"
            " there lived a princess imprisoned in the highest tower\r\n"
            " of the most haunted castle.\r\n",
            {http_header,0,  "Multi-Line"  ,undefined,  "Once upon a time in a land far far away,\r\n there lived a princess imprisoned in the highest tower\r\n of the most haunted castle."},
            {http_header,0,<<"Multi-Line">>,undefined,<<"Once upon a time in a land far far away,\r\n there lived a princess imprisoned in the highest tower\r\n of the most haunted castle.">>}},
           {"\r\n",
            http_eoh,
            http_eoh}],
    Bin = lists:foldl(fun({Line,_,_},Acc) -> LineBin = list_to_binary(Line), 
                                             <<Acc/binary,LineBin/binary>> end,
                      <<"">>, QnA),
    MsgBin = list_to_binary(Msg),
    {<<Bin/binary,MsgBin/binary>>, QnA}.


http_response(Msg) ->
    QnA = [{"HTTP/1.0 404 Object Not Found\r\n",
            {http_response, {1,0}, 404,   "Object Not Found"},
            {http_response, {1,0}, 404, <<"Object Not Found">>}},
           {"Server: inets/4.7.16\r\n",
            {http_header, 30, 'Server', undefined,   "inets/4.7.16"},
            {http_header, 30, 'Server', undefined, <<"inets/4.7.16">>}},
           {"Date: Fri, 04 Jul 2008 17:16:22 GMT\r\n",
            {http_header, 3, 'Date', undefined,   "Fri, 04 Jul 2008 17:16:22 GMT"},
            {http_header, 3, 'Date', undefined, <<"Fri, 04 Jul 2008 17:16:22 GMT">>}},
           {"Content-Type: text/html\r\n",
            {http_header, 42, 'Content-Type', undefined,   "text/html"},
            {http_header, 42, 'Content-Type', undefined, <<"text/html">>}},
           {"Content-Length: 207\r\n",
            {http_header, 38, 'Content-Length', undefined,   "207"},
            {http_header, 38, 'Content-Length', undefined, <<"207">>}},
           {"\r\n",
            http_eoh,
            http_eoh}],



    Bin = lists:foldl(fun({Line,_,_},Acc) -> LineBin = list_to_binary(Line), 
                                             <<Acc/binary,LineBin/binary>> end,
                      <<"">>, QnA),
    MsgBin = list_to_binary(Msg),
    {<<Bin/binary,MsgBin/binary>>, QnA}.

http_hdr_strings() ->
    %% Must be correct order
    ["Cache-Control","Connection","Date","Pragma","Transfer-Encoding",
     "Upgrade","Via","Accept", "Accept-Charset", "Accept-Encoding",
     "Accept-Language", "Authorization","From","Host","If-Modified-Since",
     "If-Match","If-None-Match","If-Range","If-Unmodified-Since","Max-Forwards",
     "Proxy-Authorization","Range","Referer","User-Agent","Age","Location",
     "Proxy-Authenticate","Public","Retry-After","Server","Vary","Warning",
     "Www-Authenticate","Allow","Content-Base","Content-Encoding",
     "Content-Language","Content-Length","Content-Location","Content-Md5",
     "Content-Range","Content-Type","Etag","Expires","Last-Modified",
     "Accept-Ranges","Set-Cookie","Set-Cookie2","X-Forwarded-For","Cookie",
     "Keep-Alive","Proxy-Connection"].

http_meth_strings() ->
    ["OPTIONS", "GET", "HEAD", "POST", "PUT", "DELETE", "TRACE"].

http_uri_variants() ->
    [{"*", '*', '*'},
     {"http://tools.ietf.org/html/rfc3986",
      {absoluteURI,http,  "tools.ietf.org",  undefined,  "/html/rfc3986"},
      {absoluteURI,http,<<"tools.ietf.org">>,undefined,<<"/html/rfc3986">>}},
     {"http://otp.ericsson.se:8000/product/internal/",
      {absoluteURI,http,  "otp.ericsson.se"  ,8000,  "/product/internal/"},
      {absoluteURI,http,<<"otp.ericsson.se">>,8000,<<"/product/internal/">>}},
     {"https://example.com:8042/over/there?name=ferret#nose",
      {absoluteURI,https,  "example.com",  8042,  "/over/there?name=ferret#nose"},
      {absoluteURI,https,<<"example.com">>,8042,<<"/over/there?name=ferret#nose">>}},
     {"ftp://cnn.example.com&story=breaking_news@10.0.0.1/top_story.htm",
      {scheme,  "ftp",    "//cnn.example.com&story=breaking_news@10.0.0.1/top_story.htm"},
      {scheme,<<"ftp">>,<<"//cnn.example.com&story=breaking_news@10.0.0.1/top_story.htm">>}},
     {"/some/absolute/path",
      {abs_path,  "/some/absolute/path"},
      {abs_path,<<"/some/absolute/path">>}},
     {"something_else", "something_else", <<"something_else">>}].


line(Config) when is_list(Config) ->
    Text = <<"POST /invalid/url HTTP/1.1\r\n"
             "Connection: close\r\n"
             "Host\t : localhost:8000\r\n"
             "User-Agent: perl post\r\n"
             "Content-Length: 4\r\n"
             "Content-Type: text/xml; charset=utf-8\r\n"
             "Other-Field: with some text\r\n"
             "Multi-Line: Once upon a time in a land far far away,\r\n"
             " there lived a princess imprisoned in the highest tower\r\n"
             " of the most haunted castle.\r\n"
             "\r\nThe residue">>,

    lists:foreach(fun(MaxLen) -> line_do(Text,MaxLen) end,
                  [0,7,19,29,37]),
    ok.

line_do(Bin,MaxLen) ->
    Res = decode_pkt(line,Bin,[{line_length,MaxLen}]),
    MyRes = decode_line(Bin,MaxLen),
    MyRes = Res,
    case Res of
        {ok,_,Rest} ->
            line_do(Rest,MaxLen);
        {more,undefined} ->
            ok
    end.

% Emulates decode_packet(line,Bin,[{line_length,MaxLen}])
decode_line(Bin,MaxLen) ->
    case find_in_binary($\n,Bin) of
        notfound when MaxLen>0 andalso byte_size(Bin) >= MaxLen ->
            {LineB,Rest} = split_binary(Bin,MaxLen),
            {ok,LineB,Rest};
        notfound ->
            {more,undefined};
        Pos when MaxLen>0 andalso Pos > MaxLen ->
            {LineB,Rest} = split_binary(Bin,MaxLen),
            {ok,LineB,Rest};
        Pos ->
            {LineB,Rest} = split_binary(Bin,Pos),
            {ok,LineB,Rest}
    end.

find_in_binary(Byte, Bin) ->
    case string:find(Bin, [Byte]) of
        nomatch -> notfound;
        Suffix -> byte_size(Bin) - byte_size(Suffix) + 1
    end.    

ssl(Config) when is_list(Config) ->
    Major = 34,
    Minor = 17,
    Body = <<234,189,73,199,1,32,4,0,254>>,
    Rest = <<23,123,203,12,234>>,

    F = fun(Content) ->
                {Packet,Unpacked} = pack_ssl(Content, Major, Minor, Body),
                Bin = <<Packet/binary,Rest/binary>>,
                {ok, Unpacked, Rest} = decode_pkt(ssl_tls, Bin)
        end,    
    F(25),
    F(v2hello),
    ok.

%% Corrupt sub-binary-strings from httph_bin
otp_8536(Config) when is_list(Config) ->
    lists:foreach(fun otp_8536_do/1, lists:seq(1,50)),
    ok.

otp_8536_do(N) ->
    Data = <<"some data 123">>,
    Letters = <<"bcdefghijklmnopqrstuvwxyzyxwvutsrqponmlkjihgfedcba">>,
    <<HdrTail:N/binary,_/binary>> = Letters,
    Hdr = <<$A, HdrTail/binary>>,
    Bin = <<Hdr/binary, ": ", Data/binary, "\r\n\r\n">>,

    io:format("Bin='~p'\n",[Bin]),
    {ok,{http_header,0,Hdr2,undefined,Data2},<<"\r\n">>} = decode_pkt(httph_bin, Bin,  []),

    %% Do something to trash the C-stack, how about another decode_packet:
    decode_pkt(httph_bin,<<Letters/binary, ": ", Data/binary, "\r\n\r\n">>, []),

    %% Now check that we got the expected binaries
    {Hdr, Data} = {Hdr2, Data2}.

decode_pkt(Type,Bin) ->
    decode_pkt(Type,Bin,[]).		       
decode_pkt(Type,Bin,Opts) ->
    %%io:format("decode_packet(~p,~p,~p)\n",[Type,Bin,Opts]),
    Res = erlang:decode_packet(Type,Bin,Opts),
    %%io:format(" -> ~p\n",[Res]),
    Res.

%% Verify line_length works correctly for HTTP headers
otp_9389(Config) when is_list(Config) ->
    Opts = [{packet_size, 16384}, {line_length, 3000}],
    Pkt = list_to_binary(["GET / HTTP/1.1\r\nHost: localhost\r\nLink: /",
                          lists:duplicate(8192, $X),
                          "\r\nContent-Length: 0\r\n\r\n"]),
    <<Pkt1:5000/binary, Pkt2/binary>> = Pkt,
    {ok, {http_request,'GET',{abs_path,"/"},{1,1}}, Rest1} =
    erlang:decode_packet(http, Pkt1, Opts),
    {ok, {http_header,_,'Host',_,"localhost"}, Rest2} =
    erlang:decode_packet(httph, Rest1, Opts),
    {more, undefined} = erlang:decode_packet(httph, Rest2, Opts),
    {ok, {http_header,_,"Link",_,Link}, Rest3} =
    erlang:decode_packet(httph, list_to_binary([Rest2, Pkt2]), Opts),
    true = (length(Link) > 8000),
    {ok, {http_header,_,'Content-Length',_,"0"}, <<"\r\n">>} =
    erlang:decode_packet(httph, Rest3, Opts),
    ok.

%% Verify packet_size works correctly for line mode
otp_9389_line(Config) when is_list(Config) ->
    Opts = [{packet_size, 20}],
    Line1 = <<"0123456789012345678\n">>,
    Line2 = <<"0123456789\n">>,
    Line3 = <<"01234567890123456789\n">>,
    Pkt = list_to_binary([Line1, Line2, Line3]),
    {ok, Line1, Rest1} = erlang:decode_packet(line, Pkt, Opts),
    {ok, Line2, Rest2} = erlang:decode_packet(line, Rest1, Opts),
    {error, invalid} = erlang:decode_packet(line, Rest2, Opts),
    ok.
