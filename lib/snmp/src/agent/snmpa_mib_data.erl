%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-module(snmpa_mib_data).
-moduledoc """
Behaviour module for the SNMP agent mib-server data module.

This module defines the behaviour of the SNMP agent mib-server data module. A
`snmpa_mib_data` compliant module must export the following functions:

- [new/1](`c:snmpa_mib_data:new/1`)
- [close/1](`c:snmpa_mib_data:close/1`)
- [sync/1](`c:snmpa_mib_data:sync/1`)
- [load_mib/4](`c:snmpa_mib_data:load_mib/4`)
- [unload_mib/4](`c:snmpa_mib_data:unload_mib/4`)
- [lookup/2](`c:snmpa_mib_data:lookup/2`)
- [next/3](`c:snmpa_mib_data:next/3`)
- [register_subagent/3](`c:snmpa_mib_data:register_subagent/3`)
- [unregister_subagent/2](`c:snmpa_mib_data:unregister_subagent/2`)
- [which_mib/2](`c:snmpa_mib_data:which_mib/2`)
- [which_mibs/1](`c:snmpa_mib_data:which_mibs/1`)
- [whereis_mib/2](`c:snmpa_mib_data:whereis_mib/2`)
- [dump/2](`c:snmpa_mib_data:dump/2`)
- [info/1](`c:snmpa_mib_data:info/1`)
- [backup/2](`c:snmpa_mib_data:backup/2`)
- [code_change/4](`c:snmpa_mib_data:code_change/4`)

The semantics of them and their exact signatures are explained below.

Note that the data extracted from the imported (loaded) mibs are stored partly
by the mib-server and partly by the symbolic-store server. See the default
mib-server data module, `snmpa_mib_data_tttn` for details.
""".
-moduledoc(#{since => "OTP R16B01",
             titles => [{callback,<<"CALLBACK FUNCTIONS">>}]}).

-include_lib("snmp/include/snmp_types.hrl").

%%%-----------------------------------------------------------------
%%% This is the behaviour for the MIB server backend internal 
%%% data storage. 
%%%-----------------------------------------------------------------

%% These types should really be defined elsewhere...
-export_type([
	      mib_view/0, 
	      mib_view_elem/0, 
	      mib_view_mask/0, 
	      mib_view_inclusion/0
	     ]).

-type mib_view()           :: [mib_view_elem()].
-type mib_view_elem()      :: {SubTree :: snmp:oid(), 
			       Mask :: [non_neg_integer()], 
			       Inclusion :: mib_view_inclusion()}.
-type mib_view_mask()      :: [non_neg_integer()]. 
-type mib_view_inclusion() :: 1 | 2. % 1 = included, 2 = excluded

-type filename() :: file:filename().


-doc """
Create a new mib-server data instance.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback new(MibStorage :: snmpa:mib_storage()) -> State :: term().

-doc """
Close the mib-storage.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback close(State :: term()) -> ok.

-doc """
Synchronize (write to disc, if possible) the mib-server data. This depends on
the `mib_storage` option, and will only have an effect if the mib-storage option
has an actual disc component (such as dets, or ets with a file).
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback sync(State :: term()) -> ok.

-doc """
Load the mib specified by the `Filename` argument into the mib-server. The
`MeOverride` and `TeOverride` arguments specifies how the mib-server shall
handle duplicate mib- and trap- entries.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback load_mib(State :: term(), FileName :: filename(),
		   MeOverride :: boolean(), 
		   TeOverride :: boolean()) -> 
    {ok, NewState :: term()} | {error, Reason :: already_loaded | term()}.

-doc """
Unload the mib specified by the `Filename` argument from the mib-server. The
`MeOverride` and `TeOverride` arguments specifies how the mib-server shall
handle duplicate mib- and trap- entries.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback unload_mib(State :: term(), FileName :: filename(),
		   MeOverride :: boolean(), 
		   TeOverride :: boolean()) -> 
    {ok, NewState :: term()} | {error, Reason :: not_loaded | term()}.

-doc """
Find the mib-entry corresponding to the `Oid`. If it is a variable, the `Oid`
must be <Oid for var>.0 and if it is a table, `Oid` must be
`<table>.<entry>.<col>.<any>`.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback lookup(State :: term(), Oid :: snmp:oid()) -> 
    {false, Reason :: term()} | 
    {variable, MibEntry :: snmpa:me()} |
    {table_column, MibEntry :: snmpa:me(), TableEntryOid :: snmp:oid()} |
    {subagent, SubAgentPid :: pid(), SAOid :: snmp:oid()}.

-doc """
Finds the lexicographically next oid.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback next(State :: term(), Oid :: snmp:oid(), MibView :: mib_view()) -> 
    endOfView | false | 
    {subagent, SubAgentPid :: pid(), SAOid :: snmp:oid()} |
    {variable, MibEntry :: snmpa:me(), VarOid :: snmp:oid()} |
    {table, TableOid :: snmp:oid(), TableRestOid :: snmp:oid(), MibEntry :: snmpa:me()}.

-doc """
Register the subagent, process, handling part of the mib-tree.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback register_subagent(State :: term(), 
			    Oid   :: snmp:oid(), 
			    Pid   :: pid()) -> 
    {ok, NewState :: term()} | {error, Reason :: term()}.

-doc """
Unregister the subagent, handling part of the mib-tree, as specified by the
`oid()` or `t:pid/0` (`PidOrOid`).

When unregister the subagent using an `oid()`, the `t:pid/0` of the process
handling the sub-tree is also returned.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback unregister_subagent(State :: term(), 
			      PidOrOid :: pid() | snmp:oid()) -> 
    {ok, NewState :: term()}               | % When second arg was a pid()
    {ok, NewState :: term(), Pid :: pid()} | % When second arg was a oid()
    {error, Reason :: term()}.

-doc """
Dump the mib-server data to `stdio` (Destination = `io`) or the specified file.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback dump(State :: term(), Destination :: io | filename()) -> 
    ok | {error, Reason :: term()}.

-doc """
Retrieve the mib-file to which an given `oid()` belongs.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback which_mib(State :: term(), Oid :: snmp:oid()) -> 
    {ok, Mib :: string()} | {error, Reason :: term()}.

-doc """
Retrieve all loaded mib-files.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback which_mibs(State :: term()) -> 
    [{MibName :: atom(), Filename :: filename()}].

-doc """
Retrieve the mib file for the mib.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback whereis_mib(State :: term(), MibName :: atom()) -> 
    {ok, Filename :: filename()} | {error, Reason :: term()}.

-doc """
Retrieve misc info for the mib data.

This is a utility function used to inspect, for instance, memory usage, in a
simple way.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback info(State :: term()) -> list().

-doc """
Perform a backup of the mib-server data.

Note that its implementation dependent (and also dependent on mib-storage is
used) if a backup is possible.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback backup(State :: term(), BackupDir :: string()) -> 
    ok |  {error, Reason :: term()}.

-doc """
Perform a code-change (upgrade or downgrade).

See `m:gen_server` for more info regarding the `Vsn` and `Extra` arguments.
""".
-doc(#{title => <<"CALLBACK FUNCTIONS">>,since => <<"OTP R16B01">>}).
-callback code_change(Direction :: up | down, 
		      Vsn :: term(), 
		      Extra :: term(), 
		      State :: term()) -> 
    NewState :: term().

%% Backwards-compatibility callback
-callback unload_mib(State, Filename) -> {ok, NewState} | {error, Reason} when
      State :: term(),
      Filename :: filename(),
      NewState :: term(),
      Reason :: not_loaded | term().

-optional_callbacks([unload_mib/2]).

