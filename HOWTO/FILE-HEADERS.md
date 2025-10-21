<!--
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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
-->

<!--
%% REUSE-IgnoreStart
-->


License Headers in Erlang/OTP
-----------------------------

Each file in the Erlang/OTP repository must contain a license header containing
information about which license the file is under and who owns the copyright of it.

The contents can be checked by calling `./scripts/license-header.es scan --path /path/to/file`
and needs to exactly follow the rules described in this document to pass the check.

The check of how the license headers need to look is very strict as otherwise
the layout tends to vary and the exact text in the headers have had copy-paste
mistakes.

## Standard template

The standard template to use is the following:

```
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright YYYY-yyyy Full Name <email@example.com>

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
```

If the file is under some other license, it needs to have its `SPDX-License-Identifier`
and also a copy of the license header information needs to be in `FILE-HEADERS/`.
If the entire license is to be part of the header, you should place the license
text in `LICENSES/` directly and the tool will use those. Different licenses have
different conventions regarding if the whole license should be in the header or not.
If you are adding a new license, please check what the community at large is doing
with that specific license. If it is unclear, then a general guideline is that
"small" licenses should be in the header and "large" should have a short text in
the header and a link to where you can find the license.

You can find a list of all `SPDX-License-Identifier`s on <https://spdx.org/licenses/>.

The license header can be prefixed by any characters, but it needs to be same
prefix for all lines. For example:

```
%% %CopyrightBegin%
%% 
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2025. All Rights Reserved.
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
```

the above is valid, while the example below is invalid:

```
/* %CopyrightBegin%
 * 
 * SPDX-License-Identifier: Apache-2.0
 * 
 * Copyright Ericsson AB 2025. All Rights Reserved.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * 
 * %CopyrightEnd% */
```

## Copyright statement

When creating or editing a file you should add the copyright statement of
you or the organization you represent in the license header.
The copyright notice must start with `Copyright`, and can have as prefix the SPDX annotation `SPDX-FileCopyrightText: `
followed by the holders of the copyright, as follows:

1. To be used exclusively by OTP team
   - `Copyright Ericsson AB YYYY. All Rights Reserved.`
   - `Copyright Ericsson AB YYYY-YYYY. All Rights Reserved.`
2. To be used by the community
  - `Copyright YYYY CopyrightHolder <your-email@email.com>`
  - `Copyright YYYY-YYYY CopyrightHolder <your-email@email.com>`
  - `SPDX-FileCopyrightText: Copyright YYYY CopyrightHolder <your-email@email.com>`
  - `SPDX-FileCopyrightText: Copyright YYYY-YYYY CopyrightHolder <your-email@email.com>`
  - `SPDX-FileCopyrightText: Copyright YYYY Erlang/OTP and its contributors`, default case but not recommended due to missing contact information
3. To be used exclusively in case of need in vendor applications, but not recommended as it misses the contact information.
  - `Copyright (C) YYYY CopyrightHolder` 
  - `Copyright (C) YYYY-YYYY CopyrightHolder`
  - `SPDX-FileCopyrightText: Copyright (C) YYYY CopyrightHolder`
  - `SPDX-FileCopyrightText: Copyright (C) YYYY-YYYY CopyrightHolder`

> For any contributions made by the Erlang/OTP team, the copyright statement must
> be `Copyright Ericsson AB YYYY. All Rights Reserved.`. The `license-header.es` script
> will check that this format is followed and will automatically update the copyright
> year when any changes are made.


There can be multiple "Copyright" lines if there are multiple copyright holders.

For example:

```
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2025. All Rights Reserved.
SPDX-FileCopyrightText: Copyright (C) 2019 The ORT Project Authors

Licensed under the Apache License, Version 2.0 (the "License");
...
```

## Short files

For short files (less than 20 lines long), it is allowed to not include
the full license statement. For example:

```
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2025. All Rights Reserved.

%CopyrightEnd%
...
```

## Non-text files

If the file is a binary (such as an image or archive) that cannot include
a license header, it is possible to add a `/path/to/file.license` that contains
the license header. That file should have the same format as a normal license
header, including the `%CopyrightBegin%` and `%CopyrightEnd%`.

## Vendored dependencies

The standard license header does not have to be used with vendored dependencies.
However, the files should all be [REUSE](https://reuse.software) compatible,
so that means that they have to have a `SPDX-License-Identifier` and a
copyright notice. For example:

```
// zstd.c
SPDX-License-Identifier: BSD-3-Clause OR GPL-2.0
Copyright (c) Meta Platforms, Inc. and affiliates.
```

Vendored dependencies are defined as any dependency covered by a `vendor.info`
file as described in [SBOM.md](SBOM.md#update-spdx-vendor-packages).

## Placement of license header

It is highly recommended to place the license header at the top of the file,
but sometime that is not possible so it is allowed to place it anywhere in the
file.

<!--
%% REUSE-IgnoreEnd
-->
