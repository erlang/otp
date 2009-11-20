%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
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
-module(opt_crash).
-export([?MODULE/0,test/0]).

?MODULE() ->
    ok.

test() ->
   URI_Before =
   {absoluteURI,
      {scheme,fun() -> nil end},
      {'hier-part',
         {'net-path',
            {srvr,
               {userinfo,nil},
               fun() -> nil end},
            nil},
         {'query',nil}}},

   {absoluteURI,
      {scheme,_},
      {'hier-part',
         {'net-path',
            {srvr,
               {userinfo,nil},
               HostportBefore},
            nil},
         {'query',nil}}} = URI_Before,

   %% ... some funky code ommitted, not relevant ...

   {absoluteURI,
      {scheme,_},
      {'hier-part',
         {'net-path',
            {srvr,
               {userinfo,nil},
               HostportAfter},
            nil},
         {'query',nil}}} = URI_Before,
   %% NOTE: I intended to write URI_After instead of URI_Before
   %% but the accident revealed that when you add the line below,
   %% it causes internal error in v3_codegen on compilation
   {hostport,{hostname,"HostName"},{port,nil}} = HostportAfter,

   ok.


