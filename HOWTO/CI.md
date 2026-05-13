<!--
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

# Erlang/OTP CI


Erlang/OTP uses Github Actions as a preliminary CI to check that nothing fundamental has been broken by a change.

## CI architecture
Erlang/OTP maintains several branches simultaneously: 
- patches and maintainance branches, e.g., `maint-26`, `maint-27`, and `maint-28`
- current dev branch, `maint`
- next release branch, `master` 

Each of these branches has its own CI, and historically a CI change on an older
branch would require a chain of forward merges all the way up to master to keep
things in sync.

To avoid this, all CI logic is now defined once as reusable workflows in
`master`. Each maintenance branch carries only a thin wrapper workflow that
delegates to `master`. Something similar to the following:

```yaml
name: Build and Test (maint-26)

on:
  push:
  pull_request:  

jobs:
  call-ci:
    uses: erlang/otp/.github/workflows/maint-26.yaml@master
    secrets: inherit # or something more specific if possible
```

The workflow definition is fetched from master, but the github context is
preserved entirely from the calling branch. This means that when a `push` or `pull`
request on `maint-26` triggers the CI, actions/checkout will check out the
`maint-26` code, all scripts run against `maint-26`, and `github.ref` reflects
`maint-26`. The `master` branch contributes only the workflow logic, not the code
under test.

The practical effect of this is:

- A CI fix or improvement committed to master takes effect on all branches immediately, with no forward merges required.
- Each branch wrapper is identical or nearly so, with no branch-specific logic needed in the wrapper itself.
- The CI remains a single source of truth, making it much easier to audit and improve over time.
- If a change should only affect a specific `base_branch` (e.g., `maint-28` but not `maint-26` nor `maint-27`),
  write an if-condition that skips those
  
## Conditional CI Jobs Based on Versions

Conditional steps are discouraged because they break the uniformity that makes
the shared CI approach work well. However, there are cases where two maintained
branches genuinely need different behaviour, and this section describes how to
handle those cases cleanly.

### How the OTP version is determined

The `reusable-setup.yaml` workflow reads the major OTP version directly from the
`OTP_VERSION` file in the repository:

```bash
cat OTP_VERSION | cut -d. -f1
# e.g. 29
```

It then exposes this as the output variable `otp_version`, formatted as
`maint-XX` (for example `maint-29`). Because this value is read from the branch
being tested (rather than from any input), no maintenance branch ever
needs to be updated when a new release branch is created (meaning, a `maint` branch that turns
into a `maint-XX` branch will work automatically, because any pull request or push
that triggers the CI detects the correct version for `maint-XX` from its `OTP_VERSION` file).

### When to use this

Use version-based conditions only when two branches have a genuine structural
difference that cannot be avoided, for example when a new release format or
artifact type is introduced that older release lines do not produce.

The example below shows a step that attaches attestation and SBOM files to a
release. These files are only produced from OTP 28 onwards, so the step is split
in two:

```yaml
# OTP 28 and later: includes attestations and SBOM artifacts
- name: Create release
  if: ${{ inputs.otp_version != 'maint-26' && inputs.otp_version != 'maint-27' }}
  env:
    GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    TAG:      ${{ inputs.tag }}
    VSN:      ${{ inputs.vsn }}
    LATEST:   ${{ steps.latest-release.outputs.latest }}
  run: |
    gh release create "${TAG}" --title "OTP ${VSN}" --latest=${LATEST} \
       artifacts/*.tar.gz \
       artifacts/*.txt \
       attestations/*.sigstore \
       scan-report-web-app.html \
       bom.*

# OTP 26 and 27: attestations and SBOM not available
- name: Create release (OTP 26 and 27)
  if: ${{ inputs.otp_version == 'maint-26' || inputs.otp_version == 'maint-27' }}
  env:
    GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    TAG:      ${{ inputs.tag }}
    VSN:      ${{ inputs.vsn }}
    LATEST:   ${{ steps.latest-release.outputs.latest }}
  run: |
    gh release create "${TAG}" --title "OTP ${VSN}" --latest=${LATEST} \
       artifacts/*.tar.gz \
       artifacts/*.txt
```

When OTP 26 eventually reaches end of life and `maint-26` is retired, the
corresponding condition can be removed from the reusable workflow without
touching any other branch.
