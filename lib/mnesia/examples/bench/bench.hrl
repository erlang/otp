%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File    : bench.hrl
%%% Author  : Hakan Mattsson <hakan@cslab.ericsson.se>
%%% Purpose : Define various database records
%%% Created : 21 Jun 2001 by Hakan Mattsson <hakan@cslab.ericsson.se>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(config,
        {
          generator_profile         = random,
          generator_warmup          = timer:seconds(2),
          generator_duration        = timer:seconds(15),
          generator_cooldown        = timer:seconds(2),
          generator_nodes           = [node() | nodes()],
          statistics_detail         = debug,
          n_generators_per_node     = 1,
          write_lock_type           = sticky_write,
          table_nodes               = [node() | nodes()],
          storage_type              = ram_copies,
          n_subscribers             = 25000,
          n_groups                  = 5,
          n_servers                 = 1,
          n_replicas                = 1,
          n_fragments               = 100,
          use_binary_subscriber_key = false,
	  always_try_nearest_node   = false,
          cookie                    = 'bench'
         }).

-record(subscriber,
        {
          subscriber_number, % string (10 chars)
          subscriber_name,   % string (32 chars)
          group_id,          % integer (uint32)
          location,          % integer (uint32)
          active_sessions,   % array of 32 booleans (32 bits)
          changed_by,        % string (25 chars)
          changed_time,      % string (25 chars)
          suffix
         }). 
 
-record(group, 
        {
          group_id,          % integer (uint32)
          group_name,        % string (32 chars)
          allow_read,        % array of 32 booleans (32 bits)
          allow_insert,      % array of 32 booleans (32 bits)
          allow_delete       % array of 32 booleans (32 bits)
         }).

-record(server,
        {
          server_key,        % {ServerId, SubscriberNumberSuffix}
          server_name,       % string (32 chars)
          no_of_read,        % integer (uint32)
          no_of_insert,      % integer (uint32)
          no_of_delete,      % integer (uint32)
          suffix
         }).

-record(session,
        {
          session_key,       % {SubscriberNumber, ServerId}
          session_details,   % string (4000 chars)
          suffix
        }).

-define(d(Format, Args),
        io:format("~s" ++ Format, [string:left(lists:flatten(io_lib:format("~p(~p):", [?MODULE, ?LINE])), 30, $ ) | Args])).

-define(e(Format, Args),
        begin 
            ok = error_logger:format("~p(~p): " ++ Format, [?MODULE, ?LINE | Args]),
            timer:sleep(1000)
        end).

-define(ERROR(M, F, A, R),
        ?e("~w:~w~p\n\t ->~p\n", [M, F, A, R])).

-define(APPLY(M, F, A),
        fun() ->
                case catch apply(M, F, A) of
                    ok -> {ok, ok};
                    {atomic, R} -> {ok, R};
                    {ok, R} -> {ok, R};
                    {aborted, R} -> ?ERROR(M, F, A, R);
                    {error, R} ->  ?ERROR(M, F, A, R);
                    R -> ?ERROR(M, F, A, R)
                end
        end()).
