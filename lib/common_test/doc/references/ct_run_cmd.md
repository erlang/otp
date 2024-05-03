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
# ct_run

Program used for starting Common Test from the OS command line.

## Description

The `ct_run` program is automatically installed with Erlang/OTP and the
`Common Test` application (for more information, see section
[Installation](install_chapter.md) in the User's Guide). The program accepts
different start flags. Some flags trigger `ct_run` to start `Common Test` and
pass on data to it. Some flags start an Erlang node prepared for running
`Common Test` in a particular mode.

The interface function `ct:run_test/1`, corresponding to the `ct_run` program,
is used for starting `Common Test` from the Erlang shell (or an Erlang program).
For details, see the `m:ct` manual page.

`ct_run` also accepts Erlang emulator flags. These are used when `ct_run` calls
`erl` to start the Erlang node (this makes it possible to add directories to the
code server path, change the cookie on the node, start more applications, and so
on).

With the optional flag `-erl_args`, options on the `ct_run` command line can be
divided into two groups:

- One group that `Common Test` is to process (those preceding `-erl_args`).
- One group that `Common Test` is to ignore and pass on directly to the emulator
  (those following `-erl_args`).

Options preceding `-erl_args` that `Common Test` does not recognize are also
passed on to the emulator untouched. By `-erl_args` the user can specify flags
with the same name, but with different destinations, on the `ct_run` command
line.

If flags `-pa` or `-pz` are specified in the `Common Test` group of options
(preceding `-erl_args`), relative directories are converted to absolute and
reinserted into the code path by `Common Test`. This is to avoid problems
loading user modules when `Common Test` changes working directory during test
runs. However, `Common Test` ignores flags `-pa` and `-pz` following `-erl_args`
on the command line. These directories are added to the code path normally (that
is, on specified form).

Exit status is set before the program ends. Value `0` indicates a successful
test result, `1` indicates one or more failed or auto-skipped test cases, and
`2` indicates test execution failure.

If `ct_run` is called with option `-help`, it prints all valid start flags to
`stdout`.

[](){: #ct_run }

## Run Tests from Command Line

```text
 ct_run -dir TestDir1 TestDir2 .. TestDirN |
  [-dir TestDir] -suite Suite1 Suite2 .. SuiteN
  [-group Groups1 Groups2 .. GroupsN] [-case Case1 Case2 .. CaseN]
  [-step [config | keep_inactive]]
  [-config ConfigFile1 ConfigFile2 .. ConfigFileN]
  [-userconfig CallbackModule1 ConfigString1 and CallbackModule2
   ConfigString2 and .. CallbackModuleN ConfigStringN]
  [-decrypt_key Key] | [-decrypt_file KeyFile]
  [-label Label]
  [-logdir LogDir]
  [-logopts LogOpts]
  [-verbosity GenVLevel | [Category1 VLevel1 and
   Category2 VLevel2 and .. CategoryN VLevelN]]
  [-silent_connections [ConnType1 ConnType2 .. ConnTypeN]]
  [-stylesheet CSSFile]
  [-cover CoverCfgFile]
  [-cover_stop Bool]
  [-event_handler EvHandler1 EvHandler2 .. EvHandlerN] |
  [-event_handler_init EvHandler1 InitArg1 and
   EvHandler2 InitArg2 and .. EvHandlerN InitArgN]
  [-include InclDir1 InclDir2 .. InclDirN]
  [-no_auto_compile]
  [-abort_if_missing_suites]
  [-multiply_timetraps Multiplier]
  [-scale_timetraps]
  [-create_priv_dir auto_per_run | auto_per_tc | manual_per_tc]
  [-repeat N] |
  [-duration HHMMSS [-force_stop [skip_rest]]] |
  [-until [YYMoMoDD]HHMMSS [-force_stop [skip_rest]]]
  [-basic_html]
  [-no_esc_chars]
  [-keep_logs all | NLogs]
  [-ct_hooks CTHModule1 CTHOpts1 and CTHModule2 CTHOpts2 and ..
   CTHModuleN CTHOptsN]
  [-ct_hooks_order test | config]
  [-exit_status ignore_config]
  [-help]
```

## Run Tests using Test Specification

```text
 ct_run -spec TestSpec1 TestSpec2 .. TestSpecN
  [-join_specs]
  [-config ConfigFile1 ConfigFile2 .. ConfigFileN]
  [-userconfig CallbackModule1 ConfigString1 and CallbackModule2
   ConfigString2 and .. and CallbackModuleN ConfigStringN]
  [-decrypt_key Key] | [-decrypt_file KeyFile]
  [-label Label]
  [-logdir LogDir]
  [-logopts LogOpts]
  [-verbosity GenVLevel | [Category1 VLevel1 and
   Category2 VLevel2 and .. CategoryN VLevelN]]
  [-allow_user_terms]
  [-silent_connections [ConnType1 ConnType2 .. ConnTypeN]]
  [-stylesheet CSSFile]
  [-cover CoverCfgFile]
  [-cover_stop Bool]
  [-event_handler EvHandler1 EvHandler2 .. EvHandlerN] |
  [-event_handler_init EvHandler1 InitArg1 and
   EvHandler2 InitArg2 and .. EvHandlerN InitArgN]
  [-include InclDir1 InclDir2 .. InclDirN]
  [-no_auto_compile]
  [-abort_if_missing_suites]
  [-multiply_timetraps Multiplier]
  [-scale_timetraps]
  [-create_priv_dir auto_per_run | auto_per_tc | manual_per_tc]
  [-repeat N] |
  [-duration HHMMSS [-force_stop [skip_rest]]] |
  [-until [YYMoMoDD]HHMMSS [-force_stop [skip_rest]]]
  [-basic_html]
  [-no_esc_chars]
  [-keep_logs all | NLogs]
  [-ct_hooks CTHModule1 CTHOpts1 and CTHModule2 CTHOpts2 and ..
   CTHModuleN CTHOptsN]
  [-ct_hooks_order test | config]
  [-exit_status ignore_config]
```

## Refresh HTML Index Files

```text
 ct_run -refresh_logs [-logdir LogDir] [-basic_html]
  [-keep_logs all | NLogs]
```

## Run Common Test in Interactive Mode

```erlang
 ct_run -shell
  [-config ConfigFile1 ConfigFile2 ... ConfigFileN]
  [-userconfig CallbackModule1 ConfigString1 and CallbackModule2
   ConfigString2 and .. and CallbackModuleN ConfigStringN]
  [-decrypt_key Key] | [-decrypt_file KeyFile]
```

## Start a Common Test Master Node

```text
 ct_run -ctmaster
```

## See Also

For information about the start flags, see section
[Running Tests and Analyzing Results](run_test_chapter.md) in the User's Guide.
