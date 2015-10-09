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
%%     $Id: httpd_example.erl,v 1.1 2008/12/17 09:53:34 mikpe Exp $
%%
-module(httpd_example).
-export([print/1]).
-export([get/2, post/2, yahoo/2, test1/2]).

-export([newformat/3]).
%% These are used by the inets test-suite
-export([delay/1]).


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


get(Env,[]) ->
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

post(Env,[]) ->
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

yahoo(Env,Input) ->
  "Location: http://www.yahoo.com\r\n\r\n".

default(Env,Input) ->
  [header(),
   top("Default Example"),
   "<B>Environment:</B> ",io_lib:format("~p",[Env]),"<BR>\n",
   "<B>Input:</B> ",Input,"<BR>\n",
   "<B>Parsed Input:</B> ",
   io_lib:format("~p",[httpd:parse_query(Input)]),"\n",
   footer()].

header() ->
  header("text/html").
header(MimeType) ->
  "Content-type: " ++ MimeType ++ "\r\n\r\n".

top(Title) ->
  "<HTML>
<HEAD>
<TITLE>" ++ Title ++ "</TITLE>
</HEAD>
<BODY>\n".

footer() ->
  "</BODY>
</HTML>\n".


newformat(SessionID,Env,Input)->
    mod_esi:deliver(SessionID,"Content-Type:text/html\r\n\r\n"),
    mod_esi:deliver(SessionID,top("new esi format test")),
    mod_esi:deliver(SessionID,"This new format is nice<BR>"),
    mod_esi:deliver(SessionID,"This new format is nice<BR>"),
    mod_esi:deliver(SessionID,"This new format is nice<BR>"),
    mod_esi:deliver(SessionID,footer()).

%% ------------------------------------------------------

delay(Time) when integer(Time) ->
    i("httpd_example:delay(~p) -> do the delay",[Time]),
    sleep(Time),
    i("httpd_example:delay(~p) -> done, now reply",[Time]),
    delay_reply("delay ok");
delay(Time) when list(Time) ->
    delay(httpd_conf:make_integer(Time));
delay({ok,Time}) when integer(Time) ->
    delay(Time);
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
