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
# cdv

Script to start the Crashdump Viewer from the OS command line.

## Description

The `cdv` shell script is located in directory `priv` of the Observer
application. The script is used for starting the Crashdump Viewer tool from the
OS command line.

For Windows users, `cdv.bat` is found in the same location.

## cdv \[file]

Argument `file` is optional. If not specified, a file dialog is displayed,
allowing you to select a crashdump from the file system.
