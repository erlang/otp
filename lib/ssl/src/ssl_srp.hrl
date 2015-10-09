%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Record definition for the TLS SRP protocol
%% see RFC 5054
%%----------------------------------------------------------------------

-ifndef(ssl_srp).
-define(ssl_srp, true).

-record(srp_user, {
	  generator :: binary(),
	  prime     :: binary(),
	  salt      :: binary(),
	  verifier  :: binary()
	 }).

-endif. % -ifdef(ssl_srp).
