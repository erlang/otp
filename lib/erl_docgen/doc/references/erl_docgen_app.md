<!--
%CopyrightBegin%

Copyright Ericsson AB 2023. All Rights Reserved.

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
# erl_docgen

The erl_docgen application is used to produce the OTP documentation.

## Description

The application consists of the following parts:

- **XSL** - A number of XSL files that is used to transform the xml files to
  html, pdf or man pages.

- **DTDs** - The DTDs used for the OTP documentation.

- **escripts** - Some scripts that is used to produce xml files according to OTP
  DTDs from some different input.

- **misc** - Erlang logo, javascripts and css stylesheets used in the
  documentation.
