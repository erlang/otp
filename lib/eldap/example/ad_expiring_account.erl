%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2021. All Rights Reserved.
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

expired_account(Ou) ->
  {ok,{eldap_search_result,X,[]}} = users(Ou),
  process(X).

users(Ou) ->
  {_,S}=eldap:open(["xxx.xxx.xxx.xxx"]),
  eldap:simple_bind(S,"loginname@domainname","password"),
  eldap:search(S,[{base,"OU=" ++ Ou ++ "OU=China,DC=example,DC=net"},{filter,eldap:present("objectClass")},{attributes,["cn","mail","pwdLastSet"]}]).


process( [{eldap_entry, Ou,[{"cn",[Cn]},{"pwdLastSet",[Pwd]},{"mail",[Mail]}]} | Rest] )->
  io:format("~p: ~p ~p~n",[Cn,caculate_pwddate(Pwd),Mail]),
  process(Rest);

process( [{eldap_entry, Ou,[{"cn",[Cn]},{"pwdLastSet",[Pwd]}]} | Rest] )->
  io:format("~p: ~p~n",[Cn,caculate_pwddate(Pwd)]),
  process(Rest);

process([_]) ->
  root;

process([]) ->
  ok.

caculate_pwddate(Pwd) ->
  _Date = round(erlang:list_to_integer(Pwd)/10000000 - 11644473600),
  Base = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  Seconds       = Base + _Date,
  { Date,_Time} = calendar:gregorian_seconds_to_datetime(Seconds),
  Date.
