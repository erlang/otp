%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%
-module(test_driver_load).

-export([test/1,encode/0]).

-include_lib("test_server/include/test_server.hrl").


test(0) ->
    ok;
test(N) ->
    spawn(?MODULE,encode,[]),
    test(N-1).

encode() ->
    ?line Msg = msg(),
    ?line {ok,_}=asn1_wrapper:encode('P-Record','PersonnelRecord',Msg),
    ok.

msg() ->
    {'PersonnelRecord',{'Name',"John","P","Smith"},
     "Director",
     51,
     "19710917",
     {'Name',"Mary","T","Smith"},
     [{'ChildInformation',{'Name',"Ralph","T","Smith"},"19571111"},{'ChildInformation',{'Name',"Susan","B","Jones"},"19590717"}]}.
    
