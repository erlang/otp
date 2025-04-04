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
and also a copy of the license information needs to be in `scripts/license-header-templates/`.

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
The copyright notice must start with either `Copyright ` or `SPDX-FileCopyrightText: `
followed by the holders of the copyright. It is highly recommended that you follow
the format described by the [REUSE Copyright Spec](https://reuse.software/spec-3.3/#format-of-copyright-notices).

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
SPDX-FileCopyrightText: (C) 2019 The ORT Project Authors

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

