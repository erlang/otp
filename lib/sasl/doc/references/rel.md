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
# rel

Release resource file

## Description

The _release resource file_ specifies which applications are included in a
release (system) based on Erlang/OTP.

This file is used by the functions in `m:systools` when generating start scripts
(`.script`, `.boot`) and release upgrade files (`relup`).

## File Syntax

The release resource file is to be called `Name.rel`.

The `.rel` file contains one single Erlang term, which is called a _release
specification_. The file has the following syntax:

```erlang
{release, {RelName,Vsn}, {erts, EVsn},
  [{Application, AppVsn} |
   {Application, AppVsn, Type} |
   {Application, AppVsn, IncApps} |
   {Application, AppVsn, Type, IncApps}]}.
```

- **`RelName = string()`** - Release name.

- **`Vsn = string()`** - Release version.

- **`EVsn = string()`** - ERTS version the release is intended for.

- **`Application = atom()`** - Name of an application included in the release.

- **`AppVsn = string()`** - Version of an application included in the release.

- **`Type = permanent | transient | temporary | load | none`** - Start type of
  an application included in the release.

  If `Type = permanent | transient | temporary`, the application is loaded and
  started in the corresponding way, see `m:application`.

  If `Type = load`, the application is only loaded.

  If `Type = none`, the application is not loaded and not started, although the
  code for its modules is loaded.

  Defaults to `permanent`

- **`IncApps = [atom()]`** - A list of applications that are included by an
  application included in the release. The list must be a subset of the included
  applications specified in the application resource file (`Application.app`)
  and overrides this value. Defaults to the same value as in the application
  resource file.

> #### Note {: .info }
>
> The list of applications must contain the Kernel and STDLIB applications.

## See Also

`m:application`, [`relup(4)`](relup.md), `m:systools`
