<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Mnesia Release Notes

This document describes the changes made to the Mnesia system from version to
version. The intention of this document is to list all incompatibilities as well
as all enhancements and bugfixes for every release of Mnesia. Each release of
Mnesia thus constitutes one section in this document. The title of each section
is the version number of Mnesia.

## Mnesia 4.23

### Fixed Bugs and Malfunctions

- Document `mnesia:foldl/4` and `mnesia:foldr/4`.

  Own Id: OTP-18798

- `mnesia:add_table_copy/3` no longer fails with reason system_limit when the
  node is starting.

  Own Id: OTP-18850

### Improvements and New Features

- Restore recreate of disc_only tables could crash if they had an index.

  Own Id: OTP-18843 Aux Id: GH-7766

## Mnesia 4.22.1

### Fixed Bugs and Malfunctions

- Do not delete old backup file if the new backup fails.

  Own Id: OTP-18711 Aux Id: ERIERL-963

## Mnesia 4.22

### Improvements and New Features

- Added debug statistics for active transactions.

  Own Id: OTP-18309 Aux Id: PR-6377

- The implementation has been fixed to use `proc_lib:init_fail/2,3` where
  appropriate, instead of `proc_lib:init_ack/1,2`.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-18490 Aux Id: OTP-18471, GH-6339, PR-6843

## Mnesia 4.21.4.2

### Fixed Bugs and Malfunctions

- `mnesia:add_table_copy/3` no longer fails with reason system_limit when the
  node is starting.

  Own Id: OTP-18850

## Mnesia 4.21.4.1

### Fixed Bugs and Malfunctions

- Do not delete old backup file if the new backup fails.

  Own Id: OTP-18711 Aux Id: ERIERL-963

## Mnesia 4.21.4

### Fixed Bugs and Malfunctions

- Improved consistency for dirty writes when a table was added with
  `add_table_copy/3`.

  Fixed a problem with sticky write, which could lead to inconsistent data.

  Own Id: OTP-18412

### Improvements and New Features

- Replace size/1 with either tuple_size/1 or byte_size/1

  The [`size/1`](`size/1`) BIF is not optimized by the JIT, and its use can
  result in worse types for Dialyzer.

  When one knows that the value being tested must be a tuple,
  [`tuple_size/1`](`tuple_size/1`) should always be preferred.

  When one knows that the value being tested must be a binary,
  [`byte_size/1`](`byte_size/1`) should be preferred. However,
  [`byte_size/1`](`byte_size/1`) also accepts a bitstring (rounding up size to a
  whole number of bytes), so one must make sure that the call to `byte_size/` is
  preceded by a call to [`is_binary/1`](`is_binary/1`) to ensure that bitstrings
  are rejected. Note that the compiler removes redundant calls to
  [`is_binary/1`](`is_binary/1`), so if one is not sure whether previous code
  had made sure that the argument is a binary, it does not harm to add an
  [`is_binary/1`](`is_binary/1`) test immediately before the call to
  [`byte_size/1`](`byte_size/1`).

  Own Id: OTP-18432 Aux Id:
  GH-6672,PR-6793,PR-6784,PR-6787,PR-6785,PR-6682,PR-6800,PR-6797,PR-6798,PR-6799,PR-6796,PR-6813,PR-6671,PR-6673,PR-6684,PR-6694,GH-6677,PR-6696,PR-6670,PR-6674

## Mnesia 4.21.3

### Fixed Bugs and Malfunctions

- Fixed crash which could happen during startup if too many decisions where sent
  from remote nodes.

  Own Id: OTP-18319 Aux Id: ERIERL-875

## Mnesia 4.21.2

### Fixed Bugs and Malfunctions

- Don't fill the logs if mnesia can't connect to all nodes, due to partitioned
  network.

  Own Id: OTP-18288 Aux Id: ERIERL-868

## Mnesia 4.21.1

### Fixed Bugs and Malfunctions

- Fixed `add_table_copy` which could leave a table lock if the receiving node
  went down during the operation.

  Own Id: OTP-18128 Aux Id: PR-6013

## Mnesia 4.21

### Improvements and New Features

- Documentation fixes.

  Own Id: OTP-17930

## Mnesia 4.20.4.4

### Fixed Bugs and Malfunctions

- `mnesia:add_table_copy/3` no longer fails with reason system_limit when the
  node is starting.

  Own Id: OTP-18850

## Mnesia 4.20.4.3

### Fixed Bugs and Malfunctions

- Do not delete old backup file if the new backup fails.

  Own Id: OTP-18711 Aux Id: ERIERL-963

## Mnesia 4.20.4.2

### Fixed Bugs and Malfunctions

- Don't fill the logs if mnesia can't connect to all nodes, due to partitioned
  network.

  Own Id: OTP-18288 Aux Id: ERIERL-868

- Fixed crash which could happen during startup if too many decisions where sent
  from remote nodes.

  Own Id: OTP-18319 Aux Id: ERIERL-875

## Mnesia 4.20.4.1

### Fixed Bugs and Malfunctions

- Fixed `add_table_copy` which could leave a table lock if the receiving node
  went down during the operation.

  Own Id: OTP-18128 Aux Id: PR-6013

## Mnesia 4.20.4

### Fixed Bugs and Malfunctions

- Fixed `mnesia:add_table_copy/3` so that calling it when mnesia started on
  another node does not fail or cause hanging nodes.

  Own Id: OTP-18056

## Mnesia 4.20.3

### Improvements and New Features

- Optimize locker to handle many read locks on the same record.

  Own Id: OTP-17973 Aux Id: ERIERL-772

## Mnesia 4.20.2

### Improvements and New Features

- Reduce the number of locks taken during table copying, should reduce the
  startup time on large systems.

  Own Id: OTP-17656 Aux Id: ERIERL-688

## Mnesia 4.20.1

### Fixed Bugs and Malfunctions

- Documentation and minor code cleanup.

  Own Id: OTP-17727

## Mnesia 4.20

### Fixed Bugs and Malfunctions

- Fixed that index keys was deleted for set tables when mnesia:delete_object/1
  tried to delete a non-existing record.

  Own Id: OTP-17564 Aux Id: GH-5040

### Improvements and New Features

- Optimized table loading and added `max_transfer_size` configuration parameter.

  Own Id: OTP-17508

## Mnesia 4.19.1

### Fixed Bugs and Malfunctions

- Suppression of deprecation warnings has been added to the source files of the
  Mnesia application.

  Own Id: OTP-17217

- Fixed that the backend plugin initialization is done only once.

  Own Id: OTP-17294 Aux Id: GH-4525 PR-4674

## Mnesia 4.19

### Fixed Bugs and Malfunctions

- Fixed the type spec for `disc_only_copies`.

  Own Id: OTP-17249 Aux Id: PR-4578

- Do not crash in `mnesia:change_config/2` if mnesia is stopping or starting.

  Own Id: OTP-17274 Aux Id: GH-4616

### Improvements and New Features

- Optimized table loading time for tables that are updated during the loading.

  Own Id: OTP-17271 Aux Id: PR-4575

## Mnesia 4.18.1

### Fixed Bugs and Malfunctions

- Avoid potential performance issue, if the input queue to `mnesia_tm` is long.

  Own Id: OTP-17066 Aux Id: PR-2889

## Mnesia 4.18

### Fixed Bugs and Malfunctions

- FIx mnesia delete object handling in transaction storage. In a transaction
  `mnesia:read/1` could indicate that exiting objects did not exist after
  another object was deleted.

  Own Id: OTP-16782 Aux Id: PR-2663

### Improvements and New Features

- Fixed crash during startup, which could happen if a table was deleted on
  another node.

  Own Id: OTP-16815 Aux Id: ERIERL-500

## Mnesia 4.17

### Fixed Bugs and Malfunctions

- Make `mnesia:create_table/2` return correct badarg value.

  Own Id: OTP-16072 Aux Id: PR-2320

- Fixed a bug where mnesia was sometimes not waiting during start for a commit
  decision on asymmetric transactions.

  Own Id: OTP-16634 Aux Id: PR-2610 ERL-1227

### Improvements and New Features

- Remove usage and documentation of old requests of the I/O-protocol.

  Own Id: OTP-15695

- Avoid using `rpc` calls to do table reads, which will reduce the load on rpc
  server and improve performance.

  Own Id: OTP-16189

## Mnesia 4.16.3.1

### Improvements and New Features

- Fixed crash during startup, which could happen if a table was deleted on
  another node.

  Own Id: OTP-16815 Aux Id: ERIERL-500

## Mnesia 4.16.3

### Fixed Bugs and Malfunctions

- Fixed a timing issue in uninstall fallback functionality.

  Own Id: OTP-16468 Aux Id: ERL-1151

## Mnesia 4.16.2

### Fixed Bugs and Malfunctions

- Fixed mnesia crash which could happen when trying to recover from failures in
  transactions containing `sticky_locks`.

  Own Id: OTP-16286 Aux Id: ERL-1077

- Fixed mnesia index issue. Could happen when updating records with a index
  plugin backend.

  Own Id: OTP-16291 Aux Id: ERL-1091

## Mnesia 4.16.1

### Fixed Bugs and Malfunctions

- `mnesia:add_table_copy/3` could cause a deadlock if called when a new node was
  starting.

  Own Id: OTP-15933 Aux Id: ERL-872

- Transactions with sticky locks could with async_asym transactions be committed
  in the wrong order, since asym transaction are spawned on the remote nodes.

  To fix this bug the communication protocol between mnesia nodes had to be
  updated, thus mnesia will no longer be able to connect to nodes earlier than
  mnesia-4.14 , OTP-19.0.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15979 Aux Id: ERL-768

## Mnesia 4.16

### Fixed Bugs and Malfunctions

- Optimize mnesia:read/1 if data have been written in the same transaction.

  Own Id: OTP-15550 Aux Id: PR-2029

- Fixed bugs in table index plugin handling.

  Own Id: OTP-15689 Aux Id: PR-1695 ERL-556

### Improvements and New Features

- Optimized dumping of tables with plugin backends.

  Own Id: OTP-15588 Aux Id: PR-2102

- Include stacktrace in exception if a dirty activity errors, thus if user have
  matched on the error thrown it may not match any more.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-15804 Aux Id: PR-2216

## Mnesia 4.15.6

### Fixed Bugs and Malfunctions

- Avoid overload warnings caused by a race condition.

  Own Id: OTP-15619 Aux Id: ERIERL-310

## Mnesia 4.15.5

### Fixed Bugs and Malfunctions

- Fixed type spec for `mnesia:change_config/2`.

  Own Id: OTP-15201 Aux Id: PR-1881

- When master node is set do not force a load from ram_copies replica when there
  are no available disc_copies, since that would load an empty table. Wait until
  a disk replica is available or until user explicitly force_loads the table.

  Own Id: OTP-15221 Aux Id: ERIERL-217

- Allow to add replicas even if all other replicas are down when the other
  replicas are not stored on disk.

  Own Id: OTP-15226 Aux Id: ERIERL-221

- Fixed `mnesia:delete_object/1` bug, where delete_object was deleting the
  record if it was written in the same transaction even if it was written to a
  different value.

  Own Id: OTP-15231 Aux Id: PR-1858

- Fixed a bug where the bag table index data was not deleted when objects were
  deleted.

  Own Id: OTP-15243

## Mnesia 4.15.4

### Improvements and New Features

- Calls to `erlang:get_stacktrace()` are removed.

  Own Id: OTP-14861

## Mnesia 4.15.3.1

### Fixed Bugs and Malfunctions

- When master node is set do not force a load from ram_copies replica when there
  are no available disc_copies, since that would load an empty table. Wait until
  a disk replica is available or until user explicitly force_loads the table.

  Own Id: OTP-15221 Aux Id: ERIERL-217

- Allow to add replicas even if all other replicas are down when the other
  replicase are not stored on disk.

  Own Id: OTP-15226 Aux Id: ERIERL-221

## Mnesia 4.15.3

### Fixed Bugs and Malfunctions

- Removed a quadratic behavior in startup. This change implies that backend
  plugins (if used) must be set when the schema is created or via configuration
  parameters before mnesia is started.

  Own Id: OTP-14829 Aux Id: ERIERL-84

- Bad timing could crash mnesia after a checkpoint was deactivated and
  reactivated with the same checkpoint name on different tables.

  Own Id: OTP-14841 Aux Id: ERIERL-113

## Mnesia 4.15.2

### Fixed Bugs and Malfunctions

- Fix backup error handling, the real failure reason was not returned.

  Own Id: OTP-14776 Aux Id: ERIERL-103

## Mnesia 4.15.1

### Improvements and New Features

- General Unicode improvements.

  Own Id: OTP-14462

## Mnesia 4.15

### Improvements and New Features

- Removed the wrapping of select continuations in extension plugin handling.
  This might require the user to rewrite user backend plugin if used.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-14039

## Mnesia 4.14.3

### Fixed Bugs and Malfunctions

- Fixed crash in checkpoint handling when table was deleted during backup.

  Own Id: OTP-14167

## Mnesia 4.14.2

### Fixed Bugs and Malfunctions

- A continuation returned by mnesia:select/\[14] should be reusable in
  different, non-transactional activities.

  Own Id: OTP-13944 Aux Id: PR-1184

- Fixed crash when calling block_table multiple times. Could happen when having
  locks for a long time and restarting mnesia.

  Own Id: OTP-13970 Aux Id: Seq-13198

- Change mnesia_tm process to have off-heap messages since mnesia_tm can be the
  receiver of many non-synchronized message from other nodes.

  Own Id: OTP-14074

## Mnesia 4.14.1

### Improvements and New Features

- Correct some minor documentation issues.

  Own Id: OTP-13891

## Mnesia 4.14

### Improvements and New Features

- Added experimental external backend plugin api. This adds the possibility for
  the user to write other storage backends for data, for example by using shared
  memory or ram-cached disk storage.

  The plugin api may change in future versions after being battle tested.

  Own Id: OTP-13058

## Mnesia 4.13.4

### Fixed Bugs and Malfunctions

- Mnesia transactions could hang while waiting on a response from a node who had
  stopped.

  Own Id: OTP-13423

## Mnesia 4.13.3

### Fixed Bugs and Malfunctions

- Avoid deadlock possibility in `mnesia:del_table_copy/2`

  Own Id: OTP-13284

## Mnesia 4.13.2

### Fixed Bugs and Malfunctions

- Fixed a process and file descriptor leak in mnesia:restore/2.

  Own Id: OTP-13025 Aux Id: seq12957

## Mnesia 4.13.1

### Fixed Bugs and Malfunctions

- Improved index updates to avoid a timing glitch in dirty_index_read.

  Own Id: OTP-12972

## Mnesia 4.13

### Fixed Bugs and Malfunctions

- Mnesia's dirty functions did not always exit with `{aborted, Reason}` as
  documented when an error occurred.

  Own Id: OTP-12714

- Consider file descriptors limits (emfile) as a fatal error and do not delete
  log files. Previously the error was seen as a corrupted disk and the log files
  deleted which caused data loss.

  Own Id: OTP-12807

### Improvements and New Features

- Make Mnesia DCD dump behavior at start up optional, when turned off mnesia
  loads large disc_copies tables faster.

  Own Id: OTP-12481

## Mnesia 4.12.5

### Fixed Bugs and Malfunctions

- Fixed race condition in protocol negotiation.

  Own Id: OTP-12473

### Improvements and New Features

- Grammar corrections. (Thanks to Derek Brown)

  Own Id: OTP-12400

## Mnesia 4.12.4

### Fixed Bugs and Malfunctions

- Fixed a spelling mistake in mnesia documentation.

  Own Id: OTP-12278

- Matching data with `mnesia:match_object/1` did not work as expected in some
  cases, when data was written in the same transaction before the matching was
  invoked.

  Own Id: OTP-12304 Aux Id: Seq12745

## Mnesia 4.12.3

### Fixed Bugs and Malfunctions

- Various logging fixes, including: Add run queue index to the process dump in
  crash dumps.  
  Add thread index to enomem slogan when crashing.  
  Remove error logger message for sending messages to old instances of the same
  node.

  Own Id: OTP-12115

## Mnesia 4.12.2

### Fixed Bugs and Malfunctions

- Fixed a race which could make create_table fail if a node was going down
  during the transaction.

  Own Id: OTP-12124 Aux Id: seq12694

## Mnesia 4.12.1

### Fixed Bugs and Malfunctions

- Force load table could hang when a node went away during start up.

  Own Id: OTP-11948 Aux Id: seq12585

- The time for inserting locks for a transaction with large number of locks is
  reduced significantly.

  Own Id: OTP-11981

## Mnesia 4.12

### Fixed Bugs and Malfunctions

- Some local implementations of removing the last element from a list are
  replaced by `lists:droplast/1`. Note that this requires at least `stdlib-2.0`,
  which is the stdlib version delivered in OTP 17.0. (Thanks to Hans Svensson)

  Own Id: OTP-11678

- Application upgrade (appup) files are corrected for the following
  applications:

  `asn1, common_test, compiler, crypto, debugger, dialyzer, edoc, eldap, erl_docgen, et, eunit, gs, hipe, inets, observer, odbc, os_mon, otp_mibs, parsetools, percept, public_key, reltool, runtime_tools, ssh, syntax_tools, test_server, tools, typer, webtool, wx, xmerl`

  A new test utility for testing appup files is added to test_server. This is
  now used by most applications in OTP.

  (Thanks to Tobias Schlager)

  Own Id: OTP-11744

### Improvements and New Features

- To prevent a race condition if there is a short communication problem when
  node-down and node-up events are received. They are now stored and later
  checked if the node came up just before mnesia flagged the node as down.
  (Thanks to Jonas Falkevik )

  Own Id: OTP-11497

- Added `mnesia:sync_log/0` to explicit sync mnesias transaction log.

  Own Id: OTP-11729

## Mnesia 4.11

### Fixed Bugs and Malfunctions

- Fixed a race in mnesia which could cause hanging transaction when sticky locks
  had been used. Thanks janchochol.

  Own Id: OTP-11375

- Fixed dirty_update_counter which could return ok, thanks Anton Ryabkov.

  Own Id: OTP-11485

## Mnesia 4.10

### Fixed Bugs and Malfunctions

- Fix timing issues in checkpoint creation.

  Own Id: OTP-10957

### Improvements and New Features

- Fixed a problem where the fallback BUP file is removed when calling
  mnesia:uninstall_fallback and mnesia is not started.

  Own Id: OTP-11241

## Mnesia 4.9

### Fixed Bugs and Malfunctions

- If mnesia:clear_table/2 was called during a table load on that table, the
  schema record was written to the table instead of clearing table.

  Own Id: OTP-11030 Aux Id: seq12267

### Improvements and New Features

- Optimize index creation for Mnesia set tables. Thanks to Nick Marino.

  Own Id: OTP-11103

## Mnesia 4.8

### Fixed Bugs and Malfunctions

- Use chained send_after instead of send_interval, to make decrease the number
  of messages sent after a sleep (Thanks to James Wheare)

  Own Id: OTP-10636

- Fix format of mnesia overload message (Thanks to Ahmed Omar)

  Own Id: OTP-10639

### Improvements and New Features

- Added a general framework for executing benchmarks of Erlang/OTP. Benchmarks
  for the Erlang VM and mnesia have been incorporated in the framework.

  For details about how to add more benchmarks see $ERL_TOP/HOWTO/BENCHMARKS.md
  in the source distribution.

  Own Id: OTP-10156

- Where necessary a comment stating encoding has been added to Erlang files. The
  comment is meant to be removed in Erlang/OTP R17B when UTF-8 becomes the
  default encoding.

  Own Id: OTP-10630

- Remove support for the query keyword and query expressions. Thanks to Loïc
  Hoguin.

  Own Id: OTP-10729

## Mnesia 4.7.1

### Fixed Bugs and Malfunctions

- Add tests showing that trying to delete non-existing object may corrupt the

  In case of bag tables, trying to delete a non-existing object leads to the
  index becoming corrupt. This happens if the non-existing object we try to
  delete happens to share its key and index field value with a single existing
  object in the table. Result: The index entry corresponding to the existing
  object is removed.

  Prevent index from being corrupted if a nonexistent item is deleted

  We have to ensure that we actually delete the last object with a given (key,
  index) pair before removing the index. Thanks to Bartlomiej Puzon

  Own Id: OTP-10220

## Mnesia 4.7

### Fixed Bugs and Malfunctions

- Returns the same value for mnesia_loader:disc_load_table/2 as
  mnesia_loader:net_load_table/4 if a table copy cannot be found. (Thanks to Uwe
  Dauernheim)

  Own Id: OTP-10015

### Improvements and New Features

- Improved table lock algorithm.

  Own Id: OTP-9890

## Mnesia 4.6

### Fixed Bugs and Malfunctions

- Reduce calls to phash in key_to_frag_number

  Original code calls phash 1..2 times, based on which fragment the hashed key
  targets and how many fragments exist. New code always calls phash only once.

  Add mnesia_frag_hash test (Thanks to Philip Robinson)

  Own Id: OTP-9722

- Fixed a sticky lock bug which caused mnesia:read(Tab, Key, write) return
  undefined.

  Own Id: OTP-9786

- Use the synchronous log_terms instead of alog_terms in mnesia_log:ets2dcd()

  This avoids the situation where mnesia could dump a very large ets table in
  its entirety into the message queue of the disk_log process, causing memory
  blowup and choking the disk logger. (Thanks to Richard Carlsson)

  Own Id: OTP-9804

### Improvements and New Features

- Implemented a new option to mnesia:create_table/2 which allows the user to
  assign 'ets' and 'dets' options not available in mnesia.

  Own Id: OTP-8970

## Mnesia 4.5.1

### Fixed Bugs and Malfunctions

- Fix deadlock in mnesia:del_table_copy/2.

  Own Id: OTP-9689 Aux Id: seq11927

### Improvements and New Features

- Allow schema operations when using different mnesia versions.

  Own Id: OTP-9657 Aux Id: seq11926

## Mnesia 4.5

### Fixed Bugs and Malfunctions

- Fix protocol issues. Mnesia-4.4.19 could not communicate with to older nodes.

  Own Id: OTP-9473

- XML files have been corrected.

  Own Id: OTP-9550 Aux Id: OTP-9541

### Improvements and New Features

- Dump the log even if no transactions have been invoked on local node,
  otherwise the log will grow forever with decisions from the other nodes who
  have tables on disk. Thanks Marek Majkowski.

  Own Id: OTP-9551

- Use dedicated api for clear_table, i.e. instead of match_delete use
  delete_all_objects. Thanks KukHyun Lee.

  Own Id: OTP-9558

## Mnesia 4.4.19

### Fixed Bugs and Malfunctions

- Mnesia could crash if mnesia:add_table_index/2 was invoked before the table
  was loaded on all nodes.

  Own Id: OTP-9285 Aux Id: seq11844

- Add \{majority, boolean()\} per-table option.

  With \{majority, true\} set for a table, write transactions will abort if they
  cannot commit to a majority of the nodes that have a copy of the table.
  Currently, the implementation hooks into the prepare_commit, and forces an
  asymmetric transaction if the commit set affects any table with the majority
  flag set. In the commit itself, the transaction will abort if it cannot
  satisfy the majority requirement for all tables involved in the
  transaction.(Thanks to Ulf Wiger)

  Own Id: OTP-9304

## Mnesia 4.4.18

### Fixed Bugs and Malfunctions

- Call chmod without the "-f" flag

  "-f" is a non-standard chmod option which at least SGI IRIX and HP UX do not
  support. As the only effect of the "-f" flag is to suppress warning messages,
  it can be safely omitted. (Thanks to Holger Weiß)

  Own Id: OTP-9170

- Mnesia sometimes failed to update meta-information in large systems, which
  could cause table content to be inconsistent between nodes.

  Own Id: OTP-9186 Aux Id: seq11728

## Mnesia 4.4.17

### Fixed Bugs and Malfunctions

- Calling mnesia:first/1 on empty fragmented table works. Thanks Magnus Henoch.

  Own Id: OTP-9108

- If Mnesia detects that the network is not fully connected during start, Mnesia
  will not start until all nodes are reachable.

  Own Id: OTP-9115 Aux Id: seq-11728

### Improvements and New Features

- Fix issues reported by dialyzer.

  Own Id: OTP-9107

## Mnesia 4.4.16

### Fixed Bugs and Malfunctions

- Sometimes a 'log_header' record was added to tables when invoking
  mnesia:restore/2 with the option 'recreate_tables'. Thanks Vance Shipley.

  Own Id: OTP-8960

### Improvements and New Features

- Compiler warnings were eliminated.

  Own Id: OTP-8855

## Mnesia 4.4.15

### Improvements and New Features

- Eliminated warnings for auto-imported BIF clashes.

  Own Id: OTP-8840

## Mnesia 4.4.14

### Improvements and New Features

- Added mnesia:subscribe(activity) contributed by Bernard Duggan.

  Own Id: OTP-8519

## Mnesia 4.4.13

### Fixed Bugs and Malfunctions

- Transactions could be left hanging if a node went down when invoking
  mnesia:sync_transaction/\[1,2]. Thanks Igor Ribeiro Sucupira.

  Own Id: OTP-8402

### Improvements and New Features

- Igor Ribeiro Sucupira added the option to compress data when copying tables
  between Mnesia nodes.

  Own Id: OTP-8406

## Mnesia 4.4.12

### Improvements and New Features

- The documentation is now built with open source tools (xsltproc and fop) that
  exists on most platforms. One visible change is that the frames are removed.

  Own Id: OTP-8250

## Mnesia 4.4.11

### Improvements and New Features

- Fixed duplicate results with mnesia:index_read() on ordered_set tables.
  Reported by Sam Bobroff.

  Fixed locking in mnesia:index_read() which now grabs a read table lock to
  ensure correctness, this may slow down the operation or block other processes
  trying to reach the same table.

  Calling mnesia:dump_log() could crash mnesia, Reported by Igor Ribeiro
  Sucupira.

  Own Id: OTP-8074

## Mnesia 4.4.10

### Fixed Bugs and Malfunctions

- Mnesia crashed if a qlc query was running inside a transaction when mnesia
  stopped at another node. Thanks Teemu Antti-Poika.

  Own Id: OTP-7968

- Mnesia could crash when loading local_content tables.

  Own Id: OTP-8002 Aux Id: seq11277

### Improvements and New Features

- Minor (smp) optimizations.

  Own Id: OTP-7928

## Mnesia 4.4.9

### Fixed Bugs and Malfunctions

- mnesia:clear_table/1 crashed instead of returning `{aborted,..}` if it was
  called inside a transaction.

  Own Id: OTP-7911

## Mnesia 4.4.8

### Fixed Bugs and Malfunctions

- With bad timing several api functions could return or exit with a bad error
  message when mnesia was shutting down.

  Own Id: OTP-7753 Aux Id: seq11179

- `mnesia:clear_table/1` cleared all nodes table content even if the table was
  `local_content` only type.

  Own Id: OTP-7835

## Mnesia 4.4.7

### Fixed Bugs and Malfunctions

- Disallowed match patterns ('\_', and '$n') as argument to
  `mnesia:delete_object/1` and friends.

  Own Id: OTP-7524

### Improvements and New Features

- Introduced a few new functions in Mnesia: `mnesia:read/2`, `mnesia:first/3`,
  `mnesia:last/3`, `mnesia:prev/4`, `mnesia:next/4`, `mnesia_frag:first/1`,
  `mnesia_frag:last/1`, `mnesia_frag:prev/2`, `mnesia_frag:next/2`.

  Own Id: OTP-7625

## Mnesia 4.4.6

### Fixed Bugs and Malfunctions

- `mnesia:restore/2` aborted if a `EXIT` message appeared in the client message
  queue.

  Own Id: OTP-7585 Aux Id: seq11046

## Mnesia 4.4.5

### Improvements and New Features

- mnesia:clear_table/1 does not require that all replicas of the table are
  available anymore.

  Own Id: OTP-7466 Aux Id: seq11015

## Mnesia 4.4.4

### Fixed Bugs and Malfunctions

- Mnesia did not garbage collect transaction decisions on disk based nodes if no
  transactions where made on the local node.

  Own Id: OTP-7419

## Mnesia 4.4.3

### Fixed Bugs and Malfunctions

- Table referred to by foreign key did not have node_pool properly cleaned up
  when a node was removed from the schema. Thanks Paul Mineiro.

  Own Id: OTP-7340

- Mnesia crashed and generated a core dump if a schema_transaction was running
  when mnesia stopped.

  Own Id: OTP-7378 Aux Id: seq10964

### Improvements and New Features

- It is now possible to delete a db node even when other disk resident nodes are
  down. Thanks Paul Mineiro.

  Own Id: OTP-7383

## Mnesia 4.4.2

### Fixed Bugs and Malfunctions

- Sticky locks could lead to hanging transactions.

  Own Id: OTP-7205 Aux Id: seq10793

- `mnesia:snmp_get_next_index/2` didn't work with partial index keys. Argument
  checking is now done according to documentation, in functions
  `mnesia:snmp_get_row/2`, `mnesia:snmp_get_mnesia_key/2` and
  `mnesia:snmp_get_next_index/2`. These functions now require that `RowIndex` is
  a list.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-7208

## Mnesia 4.4.1

### Fixed Bugs and Malfunctions

- Snmp index tables was not initialized correctly in `mnesia-4.4`.

  Own Id: OTP-7170 Aux Id: seq10870

### Known Bugs and Problems

- Rearranging fragmented tables is an O(N^2) operation.

  Own Id: OTP-6300

## Mnesia 4.4

### Fixed Bugs and Malfunctions

- Mnesia ignored the module argument to `mnesia:restore/2`. Thanks Paul Minerio.

  Own Id: OTP-6981

### Improvements and New Features

- Mnesia's snmp operations `snmp_get_row/2`, `snmp_get_next_index/2` and
  `snmp_get_mnesia_key/2` have been made context aware, i.e. inside a
  transaction they will compensate for table updates made in earlier in the same
  transaction. This might cause a performance drop if a lot of updates have been
  made before the invocation of these functions.

  \*** POTENTIAL INCOMPATIBILITY \***

  Own Id: OTP-6856 Aux Id: seq10671

- Introduced erlang:phash/2 as new default for fragmented tables. Already
  existing tables will continue to use whatever hash function they where using.

  Own Id: OTP-6923

- Introduced `mnesia:is_transaction/0`.

  Own Id: OTP-6995 Aux Id: seq10812

### Known Bugs and Problems

- Rearranging fragmented tables is an O(N^2) operation.

  Own Id: OTP-6300
