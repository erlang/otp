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
         {query,nil}}},

   {absoluteURI,
      {scheme,_},
      {'hier-part',
         {'net-path',
            {srvr,
               {userinfo,nil},
               HostportBefore},
            nil},
         {query,nil}}} = URI_Before,

   %% ... some funky code ommitted, not relevant ...

   {absoluteURI,
      {scheme,_},
      {'hier-part',
         {'net-path',
            {srvr,
               {userinfo,nil},
               HostportAfter},
            nil},
         {query,nil}}} = URI_Before,
   %% NOTE: I intended to write URI_After instead of URI_Before
   %% but the accident revealed that when you add the line below,
   %% it causes internal error in v3_codegen on compilation
   {hostport,{hostname,"HostName"},{port,nil}} = HostportAfter,

   ok.


