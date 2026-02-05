%%
%% %CopyrightBegin%
%% 
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2025-2025. All Rights Reserved.
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

-ifndef(socket_sctp_traffic_lib_hrl).
-define(socket_sctp_traffic_lib_hrl, true).

-include("kernel_test_lib.hrl").

-define(TLIB, socket_sctp_traffic_lib).


%% Message:
%% <<PROTO_TAG:32/integer,
%%   TYPE:32/integer,
%%   SEQ:32/integer,
%%   PAYLOAD_CHECKSUM:32/integer,
%%   PAYLOAD_LEN:32/integer,
%%   PAYLOAD/binary>>
-define(PROTO_TAG,   16#42004200).
-define(REQUEST_TAG, 16#0000FFFF).
-define(REPLY_TAG,   16#FFFF0000).

-define(MK_MSG(Type, Seq, Payload),
        <<(?PROTO_TAG):32,
          (Type):32,
          (Seq):32,
          (byte_size(Payload)):32,
          (erlang:crc32(Payload)):32,
          Payload/binary>>).
-define(MK_REQUEST(Seq, Payload), ?MK_MSG(?REQUEST_TAG, Seq, Payload)).
-define(MK_REPLY(Seq, Payload),   ?MK_MSG(?REPLY_TAG,   Seq, Payload)).

-define(SET_SNAME(STR), put(sname, (STR))).
-define(SET_DEBUG(D),   ?TLIB:set_debug((D))).

-define(DBG(F),        ?DBG(F, [])).
-define(DBG(F, A),     ?TLIB:print_dbg("~s -> " ++ F, [?FUNCTION_NAME | A])).
-define(INFO(F),       ?INFO(F, [])).
-define(INFO(F, A),    ?TLIB:print_info(F, A)).
-define(WARNING(F),    ?WARNING(F, [])).
-define(WARNING(F, A), ?TLIB:print_warning(F, A)).
-define(ERROR(F),      ?ERROR(F, [])).
-define(ERROR(F, A),   ?TLIB:print_error(F, A)).

-endif. % -ifdef(socket_sctp_traffic_lib_hrl).
