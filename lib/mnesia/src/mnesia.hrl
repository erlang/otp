%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

-define(APPLICATION, mnesia).

-define(ets_lookup(Tab, Key), ets:lookup(Tab, Key)).
-define(ets_lookup_element(Tab, Key, Pos), ets:lookup_element(Tab, Key, Pos)).
-define(ets_insert(Tab, Rec), ets:insert(Tab, Rec)).
-define(ets_delete(Tab, Key), ets:delete(Tab, Key)).
-define(ets_match_delete(Tab, Pat), ets:match_delete(Tab, Pat)).
-define(ets_match_object(Tab, Pat), ets:match_object(Tab, Pat)).
-define(ets_match(Tab, Pat), ets:match(Tab, Pat)).
-define(ets_info(Tab, Item), ets:info(Tab, Item)).
-define(ets_update_counter(Tab, Key, Incr), ets:update_counter(Tab, Key, Incr)).
-define(ets_first(Tab), ets:first(Tab)).
-define(ets_next(Tab, Key), ets:next(Tab, Key)).
-define(ets_last(Tab), ets:last(Tab)).
-define(ets_prev(Tab, Key), ets:prev(Tab, Key)).
-define(ets_slot(Tab, Pos), ets:slot(Tab, Pos)).
-define(ets_new_table(Tab, Props), _ = ets:new(Tab, Props)).
-define(ets_delete_table(Tab), ets:delete(Tab)).
-define(ets_fixtable(Tab, Bool), ets:fixtable(Tab, Bool)).


-define(SAFE(OP), try (OP) catch error:_ -> ok end).
-define(CATCH(OP), try (OP) catch _:_Reason -> {'EXIT', _Reason} end).

-define(catch_val(Var), (try ?ets_lookup_element(mnesia_gvar, Var, 2)
			 catch error:_ -> {'EXIT', {badarg, []}} end)).

%% It's important that counter is first, since we compare tid's

-record(tid, 
        {counter,         %% serial no for tid
         pid}).           %%  owner of tid


-record(tidstore,         
        {store,           %% current ets table for tid
         up_stores = [],  %% list of upper layer stores for nested trans
         level = 1}).     %% transaction level

-define(unique_cookie, {{erlang:monotonic_time() + erlang:time_offset(),
			 erlang:unique_integer(),1},
			node()}).

-record(cstruct, {name,                            % Atom
		  type = set,                      % set | bag
		  ram_copies = [],                 % [Node]
		  disc_copies = [],                % [Node]
		  disc_only_copies = [],           % [Node]
                  external_copies = [],            % [{{Alias,Mod},[Node]}]
		  load_order = 0,                  % Integer
		  access_mode = read_write,        % read_write | read_only
		  majority = false,                % true | false
		  index = [],                      % [Integer]
		  snmp = [],                       % Snmp Ustruct
		  local_content = false,           % true | false
		  record_name = {bad_record_name}, % Atom (Default = Name)
		  attributes = [key, val],         % [Atom]
		  user_properties = [],            % [Record]
		  frag_properties = [],            % [{Key, Val]
		  storage_properties = [],         % [{Key, Val]
                  cookie = ?unique_cookie,         % Term
                  version = {{2, 0}, []}}).        % {{Integer, Integer}, [Node]}

%% Record for the head structure in Mnesia's log files
%% 
%% The definition of this record may *NEVER* be changed
%% since it may be written to very old backup files.
%% By holding this record definition stable we can be
%% able to comprahend backups from timepoint 0. It also
%% allows us to use the backup format as an interchange
%% format between Mnesia releases.

-record(log_header,{log_kind,
		    log_version,
		    mnesia_version,
		    node,
		    now}).

%% Commit records stored in the transaction log
-record(commit, {node,
		 decision, % presume_commit | Decision
		 ram_copies = [],
		 disc_copies = [],
		 disc_only_copies = [],
		 ext = [],
		 schema_ops = []
		}).

-record(decision, {tid,
		   outcome, % presume_abort | committed
		   disc_nodes,
		   ram_nodes}).

%% Maybe cyclic wait
-record(cyclic, {node = node(),
		 oid,  % {Tab, Key}
		 op,   % read | write
		 lock, % read | write
		 lucky
		}).

%% Managing conditional debug functions

-ifdef(debug).
    -define(eval_debug_fun(I, C),
	    mnesia_lib:eval_debug_fun(I, C, ?FILE, ?LINE)).
-else.
    -define(eval_debug_fun(I, C), ok).
-endif.    

