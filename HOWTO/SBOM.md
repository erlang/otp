<!--
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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

# Software Bill-of-Materials (SBOM)

A Software Bill-of-Materials (SBOM) is a document to share information about the
software used, dependencies, and essentially what the software is made of. SBOMs have many
different use cases, and some examples include checking license compliance,
dependencies for vulnerabilities using databases such as
[CVE](https://www.cve.org/) and [OSV](https://osv.dev/), among others.

Erlang/OTP has multiple third-party dependencies. Some are vendored into the 
source code of Erlang/OTP:
- pcre (`erts/emulator/pcre`)
- zlib (`erts/emulator/zlib`)
- asmjit (`erts/emulator/asmjit`)
- openssl (`erts/emulator/openssl`)
- zstd (`erts/emulator/zstd`)
- others

The Erlang/OTP project provides source SBOMs starting with OTP-28. Below we detail
the steps necessary to run yourself the generation of the Erlang/OTP source SBOM.

## Source Software Bill-of-Materials

Erlang/OTP provides a source SBOM in each release. Here we provide details of
how to generate yourself an Erlang/OTP source SBOM.

### Erlang/OTP source SBOM

The simplest way to generate a source SBOM for Erlang/OTP is to use [oss-review-toolkit]() (ORT)
with the configuration files found in this repo (`.ort/config/config.yml` and `.ort.yml`).

- `.ort/config/config.yml` contains configuration information towards ORT, e.g., 
  configuration of the scanner to use, advisor, etc.
- `.ort.yml` contains configuration information for this specific project, e.g.,
  which files to exclude from scanner, curation of licenses, etc.

To run ORT locally, we detail the steps running ORT from source and from Docker.

#### Steps From Source

1. Install `oss-review-toolkit` (ORT) and `scancode`.
2. Run the ORT analyzer (some paths may need tweaking depending on where you are):

   ```bash
   ./gradlew cli:run --args="-c .ort/config/config.yml analyze -i . -o . -f JSON --repository-configuration-file=.ort.yml"
   ```
   
3. Run the ORT scanner (`scancode` is needed):

   ```
   ./gradlew cli:run --args="-c .ort/config/config.yml scan -o . -f JSON -i analyzer-result.json"
   ```
   
4. Generate ORT SPDX report
   ```
   ./gradlew cli:run --args="report -i cli/scan-result.json -o . -f SpdxDocument -O SpdxDocument=outputFileFormats=JSON"
   ```
   
5. From the Erlang/OTP repo, run the following escript to fix some known issues from the generated SPDX:

   ```bash
   .github/scripts/otp-compliance.es sbom otp-info --sbom-file ort/cli/bom.spdx.json --input-file ort/cli/scan-result.json
   ```

#### Steps From Docker

1. Run the ORT analyzer (some paths may need tweaking depending on where you are) and choose an appropriate version, in this example `51.0.0`:
   
   ```bash
   docker run -v $(PWD):/sbom ghcr.io/oss-review-toolkit/ort:51.0.0 -c .ort/config/config.yml analyze -i . -o . -f JSON --repository-configuration-file=.ort.yml
   ```
   
2. Run the ORT scanner:
   
   ```bash
   docker run -v $(PWD):/sbom ghcr.io/oss-review-toolkit/ort:51.0.0 -c .ort/config/config.yml scan -o . -f JSON -i analyzer-result.json
   ```
   
3. Generate ORT SPDX report

   ```bash
   docker run -v $(PWD):/sbom ghcr.io/oss-review-toolkit/ort:51.0.0 report -i cli/scan-result.json -o . -f SpdxDocument -O SpdxDocument=outputFileFormats=JSON
   ```
   
4. From the Erlang/OTP repo, run the following escript to fix some known issues from the generated SPDX,
   and to separate Erlang/OTP applications into their own SPDX Packages:
   
   ```bash
   .github/scripts/otp-compliance.es sbom otp-info --sbom-file ort/cli/bom.spdx.json --input-file ort/cli/scan-result.json
   ```

### Erlang/OTP SBOM Generation on Each Release

Erlang/OTP generates and uploads to sigstore the generated SBOM via Github Actions.
All the details can be found in `otp/.github/workflows/main.yaml`.

## Testing Erlang/OTP SBOM

The generated source SBOM should be verified. The current escript contains some basic
verification capabilities and that be complemented by other tools, such as `ntia-conformance-checker`.

To verify that the SBOM is correct, run the following escript after [Steps From Source] or [Steps From Docker]:

```bash
.github/scripts/otp-compliance.es sbom test-file --sbom-file otp.spdx.json
```

## Updating SPDX Packages

Details for how to update Erlang/OTP applications and vendor dependencies follow.

### Update Erlang/OTP Applications

**NOTE**: The source SBOM that you are generating will be based on the current
Erlang version that runs `otp-compliance.es`. This is simply because the version of
some dependencies cannot be known beforehand.

The escript `otp-compliance.es` detects Erlang/OTP applications using a simple
heuristic. Anything that contains an `.app.src` will be place in its own SPDX
Package. To detect the correct version for each Erlang application, you must ensure that
`application:get_all_key(AppName)` can run without issues. 

For example, one cannot expect `wx` application to work if the system was not
build with the required dependencies.

Updating the description of the `.app.src` package and re-building the SBOM will
make the new description to be placed in the generated source SBOM.

If there is a new Erlang/OTP release, the system that runs `otp-compliance.es`
must be the one that contains the new version.

### Add a New Erlang/OTP Application

Any new Erlang/OTP application must simply contain a `.app.src` file to
be placed as a new SPDX Package. Failing to do so will still add the files making
up the new application, but these files will not be part of their own SPDX Package.

### Delete an Erlang/OTP Application

Delete the code and any remaining `.app.src` files. Make sure that the application was
not part of a dependency in other `.app.src` files.

Re-run the source SBOM generation steps ([Erlang/OTP source SBOM]). 

### Update SPDX Vendor Packages

Vendor packages are identified by a JSON `vendor.info` file that contains fields to identify the vendor dependency. 

Each `vendor.info` file will implicitly generate a [SPDX](https://spdx.dev/) Package (within the source SBOM) to separate vendor libraries from Erlang/OTP applications.

This file may be a list of JSON objects. For simplicity, we document the fields using a JSON object.

```json
[
  {
    "ID": "erts-asmjit",
    "description": "Asmjit library",
    "copyrightText": "Copyright (c) 2008-2023 The AsmJit Authors",
    "downloadLocation": "https://github.com/asmjit/asmjit",
    "homepage": "https://github.com/asmjit/asmjit",
    "licenseDeclared": "Zlib",
    "name": "asmjit",
    "versionInfo": "029075b84bf0161a761beb63e6eda519a29020db",
    "sha": "029075b84bf0161a761beb63e6eda519a29020db",
    "path": "./erts/emulator/asmjit",
    "exclude": ["./erts/emulator/asmjit/vendor.info"],
    "supplier": "Person: Petr Kobalicek",
    "purl": "pkg:github/asmjit/asmjit",
    "sha": "029075b84bf0161a761beb63e6eda519a29020db",
    "update": "./erts/emulator/asmjit/update.sh"
  }
]
```

Fields summary:
- `ID`: represents the `id` of the third party using the following format: `<SPDX-TOP-LEVEL-PACKAGE>-<VENDOR-ID>`. 
        In the SPDX generation, `SPDX-TOP-LEVEL-PACKAGE` indicates the SPDX Package under which this vendor is a part of. For example, we write `erts-asmjit` for the vendor library `asmjit` that is a part of the `erts` SPDX Package. 
        Top-level packages are:
        - `erts`
        - All Erlang apps, e.g., `stdlib`, `ssl`, `common_test`, etc.
        - If you are unsure about the name of the `SPDX-TOP-LEVEL-PACKAGE`, take a look at the source SBOM to identify packages (under key `packages` in the SBOM).
- `description`: a brief description of what this vendor library does.
- `copyrightText`: copyright text associated with the top-level package/library/3pp using [SPDX License Identifiers](https://spdx.org/licenses/).
- `downloadLocation`: URI of the vendor library to download. If using Github, use preferably `https//` rather than `git+https//` or similars.
   This is because the download location is used for vulnerability scanning in `.github/scripts/otp-compliance.es`.
- `homepage`: homepage of the vendor library.
- `licenseDeclared`: license as declared by the vendor, following a [SPDX license identifier](https://spdx.org/licenses/).
- `name`: name of the library.
- `versionInfo`: version of the library/project/3pp. In case of no version number being available, write the commit sha.
- `sha`: sha commit for `versionInfo`, they need to be updated together!
- `ecosystem`: List of valid ecosystems in [OSV Ecosystems](https://ossf.github.io/osv-schema/#defined-ecosystems)
  where this value is omitted for C/C++ code (e.g., `asmjit`, `pcre2`, `zlib`, `zstd`, etc), and used in `vendor.json` for `jquery`.
- `path`: path to the vendor library inside Erlang/OTP. This can point to a folder or a list of files.
  - Folder: any file inside the folder is considered part of the vendor library (e.g., asmjit [vendor.info](../erts/emulator/asmjit/vendor.info)).
  - List of files: only the files listed here are part of a vendor library (e.g., erts-config [vendor.info](../erts/autoconf/vendor.info)).
- `exclude`: excludes the listed files or directories from what is listed in `path`. That is, they will not included into the generated SPDX package, but will instead be part of the SPDX package which this package is part of.
- `comment`: any comment.
- `supplier`: supplier of the software, following this standard:
  - `Person: <person name> (<email>)`, where `email` is optional.
  - `Organization: <Organization name> (email)`, where `email` is optional.
  - `NOASSERTION`, where the person adding this information (you) could not reach a reasonable conclusion, the person adding this information made no attempt to determine this field, or this field was left with no information and no meaning should be implied by doing so.
- `purl`: if the vendor has a specific `purl`, we choose this format. Otherwise, we follow the guidelines from the [PURL Specification](https://github.com/package-url/purl-spec/blob/main/PURL-TYPES.rst).
- `update`: the path to a script to be used to update the dependency. This script is called by renovate when a new version of a dependency is found.

To update the package, perform any modifications in a `vendor.info` package
and re-run the source SBOM generation steps ([Erlang/OTP source SBOM]).

### Add a New Vendor Dependency

Follow the same steps as in [Update SPDX Vendor Packages].
When running the SBOM generator, make sure to check that the new vendor dependency exists in its own package.

The [`renovate.json5`](../renovate.json5) file also needs to be updated
to make sure that the new vendored dependency gets updated as it should.

### Delete a Vendor Application

Delete the code and any remaining `vendor.info` files. 
Re-run the source SBOM generation steps ([Erlang/OTP source SBOM]). 

Delete the proper sections in [`renovate.json5`](../renovate.json5).
