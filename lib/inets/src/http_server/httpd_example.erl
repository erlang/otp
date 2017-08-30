%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-export([print/1]).
-export([get/2, put/2, post/2, yahoo/2, test1/2, get_bin/2, peer/2]).

-export([newformat/3]).
%% These are used by the inets test-suite
-export([delay/1, chunk_timeout/3]).


print(String) ->
  [header(),
   top("Print"),
   String++"\n",
   footer()].

test1(Env, []) ->
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


get(_Env,[]) ->
  [header(),
   top("GET Example"),
   "<FORM ACTION=\"/cgi-bin/erl/httpd_example:get\" METHOD=GET>	
<B>Input:</B> <INPUT TYPE=\"text\" NAME=\"input1\">
<INPUT TYPE=\"text\" NAME=\"input2\">
<INPUT TYPE=\"submit\"><BR>
</FORM>" ++ "\n",
   footer()];

get(Env,Input) ->
  default(Env,Input).

put(Env,{Input,_Body}) ->
    default(Env,Input);
put(Env,Input) ->
    default(Env,Input).

get_bin(_Env,_Input) ->
    [list_to_binary(header()),
     list_to_binary(top("GET Example")),
     list_to_binary("<FORM ACTION=\"/cgi-bin/erl/httpd_example:get\" METHOD=GET>	
<B>Input:</B> <INPUT TYPE=\"text\" NAME=\"input1\">
<INPUT TYPE=\"text\" NAME=\"input2\">
<INPUT TYPE=\"submit\"><BR>
</FORM>" ++ "\n"),
   list_to_binary(footer())].

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

yahoo(_Env,_Input) ->
  "Location: http://www.yahoo.com\r\n\r\n".

default(Env,Input) ->
  [header(),
   top("Default Example"),
   "<B>Environment:</B> ",io_lib:format("~p",[Env]),"<BR>\n",
   "<B>Input:</B> ",Input,"<BR>\n",
   "<B>Parsed Input:</B> ",
   io_lib:format("~p",[httpd:parse_query(Input)]),"\n",
   footer()].

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

    
newformat(SessionID, _Env, _Input)->
    mod_esi:deliver(SessionID, "Content-Type:text/html\r\n\r\n"),
    mod_esi:deliver(SessionID, top("new esi format test")),
    mod_esi:deliver(SessionID, "This new format is nice<BR>"),
    mod_esi:deliver(SessionID, "This new format is nice<BR>"),
    mod_esi:deliver(SessionID, "This new format is nice<BR>"),
    mod_esi:deliver(SessionID, footer()).
    
%% ------------------------------------------------------

delay(Time) when is_integer(Time) ->
    i("httpd_example:delay(~p) -> do the delay",[Time]),
    sleep(Time),
    i("httpd_example:delay(~p) -> done, now reply",[Time]),
    delay_reply("delay ok");
delay(Time) when is_list(Time) ->
    delay(list_to_integer(Time));
delay({error,_Reason}) ->
    i("delay -> called with invalid time"),
    delay_reply("delay failed: invalid delay time").

delay_reply(Reply) ->
    [header(),
     top("delay"),
     Reply,
     footer()].

i(F)   -> i(F,[]).
i(F,A) -> io:format(F ++ "~n",A).

sleep(T) -> receive after T -> ok end.

%% ------------------------------------------------------

chunk_timeout(SessionID, _, _StrInt) ->
    mod_esi:deliver(SessionID, "Tranfer-Encoding:chunked/html\r\n\r\n"),
    mod_esi:deliver(SessionID, top("Test chunk encoding timeout")),
    timer:sleep(20000),
    mod_esi:deliver(SessionID, footer()).
