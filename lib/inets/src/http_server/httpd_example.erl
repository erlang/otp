%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
-module(httpd_example).

-export([print/3, 
         get/3, 
         put/3, 
         post/3, 
         yahoo/3, 
         test1/3, 
         get_bin/3, 
         peer/3, 
         new_status_and_location/3,
         newformat/3, 
         post_chunked/3, 
         post_204/3, 
         ignore_invalid_header/3, 
         delay/3, 
         chunk_timeout/3, 
         get_chunks/3]).

%% ------------------------------------------------------
print(SessionID, _Env, Input) ->
    mod_esi:deliver(SessionID, print(Input)).

print(String) ->
  [header(),
   top("Print"),
   String++"\n",
   footer()].

%% ------------------------------------------------------
test1(SessionID, Env, _Input) ->
    mod_esi:deliver(SessionID, test1(Env)).

test1(Env) ->
    io:format("Env:~p~n",[Env]),
    ["<html>",
     "<head>",
     "<title>Test1</title>",
     "</head>",
     "<body>",
     "<h1>Erlang Body</h1>",
     "<h2>Stuff</h2>",
     "</body>",
     "</html>"].
%% ------------------------------------------------------
get(SessionID, Env, Input) ->
    mod_esi:deliver(SessionID, do_get(Env, Input)).
do_get(_Env,[]) ->
  [header(),
   top("GET Example"),
   "<FORM ACTION=\"/cgi-bin/erl/httpd_example:get\" METHOD=GET>	
<B>Input:</B> <INPUT TYPE=\"text\" NAME=\"input1\">
<INPUT TYPE=\"text\" NAME=\"input2\">
<INPUT TYPE=\"submit\"><BR>
</FORM>" ++ "\n",
   footer()];
do_get(Env,Input) ->
  default(Env,Input).
%% ------------------------------------------------------
put(SessionID, Env, Input) ->
    mod_esi:deliver(SessionID, do_put(Env, Input)).

do_put(Env,{Input,_Body}) ->
    default(Env,Input);
do_put(Env,Input) ->
    default(Env,Input).
%% ------------------------------------------------------
get_bin(SessionID, Env, Input) ->
    Header = header(),
    IoData = get_bin(Env, Input),
    Size = erlang:iolist_size(IoData),        
    mod_esi:deliver(SessionID, ["Content-Length:" ++ erlang:integer_to_list(Size) ++ "\r\n", 
                                Header, IoData]).

get_bin(_Env,_Input) ->
    [list_to_binary(top("GET Example")),
     list_to_binary("<FORM ACTION=\"/cgi-bin/erl/httpd_example:get\" METHOD=GET>	
<B>Input:</B> <INPUT TYPE=\"text\" NAME=\"input1\">
<INPUT TYPE=\"text\" NAME=\"input2\">
<INPUT TYPE=\"submit\"><BR>
</FORM>" ++ "\n"),
   list_to_binary(footer())].
%% ------------------------------------------------------
post(SessionID, Env, Input) ->
    mod_esi:deliver(SessionID, post(Env, Input)).

post(_Env,[]) ->
  [header(),
   top("POST Example"),
   "<FORM ACTION=\"/cgi-bin/erl/httpd_example:post\" METHOD=POST>	
<B>Input:</B> <INPUT TYPE=\"text\" NAME=\"input1\">
<INPUT TYPE=\"text\" NAME=\"input2\">
<INPUT TYPE=\"submit\"><BR>
</FORM>" ++ "\n",
   footer()];

post(Env,Input) ->
  default(Env,Input).
%% ------------------------------------------------------
yahoo(SessionID, Env, Input) ->
    mod_esi:deliver(SessionID, yahoo(Env, Input)).

yahoo(_Env,_Input) ->
  "Location: http://www.yahoo.com\r\n\r\n".
%% ------------------------------------------------------
new_status_and_location(SessionID, Env, Input) ->
    mod_esi:deliver(SessionID, new_status_and_location(Env, Input)).

new_status_and_location(_Env,_Input) ->
  "status:201 Created\r\n Location: http://www.yahoo.com\r\n\r\n".
%% ------------------------------------------------------

peer(SessionID, Env, Input) ->
    mod_esi:deliver(SessionID, peer(Env, Input)).

peer(Env, _Input) ->
   Header = 
     case proplists:get_value(peer_cert, Env) of
       undefined ->
   	 header("text/html", "Peer-Cert-Exist:false");
      _ ->
         header("text/html", "Peer-Cert-Exist:true")
     end,
   [Header,
   top("Test peer_cert environment option"),
   "<B>Peer cert:</B> ",
   io_lib:format("~p",[proplists:get_value(peer_cert, Env)]),"\n",
   footer()].	   	 

%% ------------------------------------------------------

post_chunked(_SessionID, _Env, {first, _Body} = _Bodychunk) ->
    {continue, {state, 1}};
post_chunked(_SessionID, _Env, {continue, _Body, {state, N}} = _Bodychunk) ->
    {continue, {state, N+1}};
post_chunked(SessionID, _Env, {last, _Body, {state, N}} = _Bodychunk) ->
    mod_esi:deliver(SessionID, "Content-Type:text/html\r\n\r\n"),
    mod_esi:deliver(SessionID, top("Received chunked body")),
    mod_esi:deliver(SessionID, "Received" ++ integer_to_list(N) ++ "chunks"),
    mod_esi:deliver(SessionID, footer());
post_chunked(SessionID, _Env, {last, _Body, undefined} = _Bodychunk) ->
    mod_esi:deliver(SessionID, "Content-Type:text/html\r\n\r\n"),
    mod_esi:deliver(SessionID, top("Received chunked body")),
    mod_esi:deliver(SessionID, "Received 1 chunk"),
    mod_esi:deliver(SessionID, footer());
post_chunked(_, _, _Body) ->
    exit(body_not_chunked).
%% ------------------------------------------------------

post_204(SessionID, _Env, _Input) ->
    mod_esi:deliver(SessionID,
                    ["Status: 204 No Content" ++ "\r\n\r\n"]),
    mod_esi:deliver(SessionID, []).
%% ------------------------------------------------------

ignore_invalid_header(SessionID, Env, _Input) ->
    case proplists:get_value(content_length, Env, undefined) of
        undefined ->
            mod_esi:deliver(SessionID,
                            ["Status: 204 No Content" ++ "\r\n\r\n"]);
        _ -> %% Invalid content_length header should have been ignored
            mod_esi:deliver(SessionID,
                            ["Status: 500 Internal Server Error" ++ "\r\n\r\n"])
    end.            
%% ------------------------------------------------------
                       
newformat(SessionID,_,_) ->
    mod_esi:deliver(SessionID, "Content-Type:text/html\r\n\r\n"),
    mod_esi:deliver(SessionID, top("new esi format test")),
    mod_esi:deliver(SessionID, "This new format is nice<BR>"),
    mod_esi:deliver(SessionID, "This new format is nice<BR>"),
    mod_esi:deliver(SessionID, "This new format is nice<BR>"),
    mod_esi:deliver(SessionID, footer()).
 
%% ------------------------------------------------------

delay(SessionID,_, _) ->
    sleep(10000),
    Reply = delay_reply("delay ok"),
    mod_esi:deliver(SessionID, Reply).

delay_reply(Reply) ->
    [header(),
     top("delay"),
     Reply,
     footer()].
%% ------------------------------------------------------

chunk_timeout(SessionID, _, _StrInt) ->
    mod_esi:deliver(SessionID, "Tranfer-Encoding:chunked/html\r\n\r\n"),
    mod_esi:deliver(SessionID, top("Test chunk encoding timeout")),
    timer:sleep(20000),
    mod_esi:deliver(SessionID, footer()).

get_chunks(Sid, _Env, In) ->
    Tokens = string:tokens(In, [$&]),
    PropList = lists:map(fun(E) ->
                                 list_to_tuple(string:tokens(E,[$=])) end,
                         Tokens),
    HeaderDelay =
        list_to_integer(proplists:get_value("header_delay", PropList, "0")),
    ChunkDelay =
        list_to_integer(proplists:get_value("chunk_delay", PropList, "0")),
    BadChunkDelay =
        list_to_integer(proplists:get_value("bad_chunk_delay", PropList, "0")),
    timer:sleep(HeaderDelay),
    mod_esi:deliver(Sid, ["Content-Type: text/plain\r\n\r\n"]),
    mod_esi:deliver(Sid, "Chunk 0 ms\r\n"),
    timer:sleep(ChunkDelay),
    mod_esi:deliver(Sid, io_lib:format("Chunk ~p ms\r\n", [ChunkDelay])),
    timer:sleep(ChunkDelay + BadChunkDelay),
    mod_esi:deliver(Sid, "BAD Chunk\r\n").

%% ------------------------------------------------------
default(Env,Input) ->
  [header(),
   top("Default Example"),
   "<B>Environment:</B> ",io_lib:format("~p",[Env]),"<BR>\n",
   "<B>Input:</B> ",Input,"<BR>\n",
   "<B>Parsed Input:</B> ",
   io_lib:format("~p",[uri_string:dissect_query(Input)]),"\n",
   footer()].

header() ->
  header("text/html").
header(MimeType) ->
  "Content-type: " ++ MimeType ++ "\r\n\r\n".
header(MimeType, Other) ->
  "Content-type: " ++ MimeType ++ "\r\n" ++ Other ++ "\r\n\r\n".			 

top(Title) ->
  "<HTML>
<HEAD>
<TITLE>" ++ Title ++ "</TITLE>
</HEAD>
<BODY>\n".

footer() ->
  "</BODY>
</HTML>\n".

sleep(T) -> receive after T -> ok end.

