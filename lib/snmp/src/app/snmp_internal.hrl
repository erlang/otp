%% 
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2006-2025. All Rights Reserved.
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

-ifndef(snmp_internal).
-define(snmp_internal, true).

-ifndef(APPLICATION).
-define(APPLICATION, snmp).
-endif.


-define(SNMP_RAND_SEED_ALG,    exrop).
-define(SNMP_RAND_SEED(),      rand:seed(?SNMP_RAND_SEED_ALG,
                                         {erlang:phash2([node()]),
                                          erlang:monotonic_time(),
                                          erlang:unique_integer()})).



%% Nicked from the inet_int.hrl (kernel internal) include file:

%% macro for guards only that checks IP address {A,B,C,D}
%% that returns true for an IP address, but returns false
%% or crashes for other terms
-define(ip4(A,B,C,D),
        (((A) bor (B) bor (C) bor (D)) band (bnot 16#ff)) =:= 0).
%% d:o for IP address as one term
-define(ip4(Addr),
        (tuple_size(Addr) =:= 4 andalso
         ?ip4(element(1, (Addr)), element(2, (Addr)),
              element(3, (Addr)), element(4, (Addr))))).
%% d:o IPv6 address
-define(ip6(A,B,C,D,E,F,G,H), 
        (((A) bor (B) bor (C) bor (D) bor (E) bor (F) bor (G) bor (H)) 
         band (bnot 16#ffff)) =:= 0).
-define(ip6(Addr),
        (tuple_size(Addr) =:= 8 andalso
         ?ip6(element(1, (Addr)), element(2, (Addr)),
              element(3, (Addr)), element(4, (Addr)),
              element(5, (Addr)), element(6, (Addr)),
              element(7, (Addr)), element(8, (Addr))))).
-define(port(P), (((P) band bnot 16#ffff) =:= 0)).


-define(snmp_info(C, F, A),    ?snmp_msg(info_msg, C, F, A)).
-define(snmp_warning(C, F, A), ?snmp_msg(warning_msg, C, F, A)).
-define(snmp_error(C, F, A),   ?snmp_msg(error_msg, C, F, A)).

-define(snmp_msg(Func, Component, Format, Args),
%% 	io:format("[ ~w : ~s : ~w : ~p ] ~n" ++ Format ++ "~n",
%% 		  [?APPLICATION, Component, ?MODULE, self() | Args]),
	(catch error_logger:Func("[ ~w : ~s : ~w : ~p ] ~n" ++ Format ++ "~n",
		[?APPLICATION, Component, ?MODULE, self() | Args]))).

-endif. % -ifdef(snmp_internal).

