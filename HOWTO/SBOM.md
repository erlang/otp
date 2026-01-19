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

The Erlang/OTP project provides source SBOMs starting with OTP 28. Below we detail
the steps necessary to run yourself the generation of the Erlang/OTP source SBOM.

For information about the structure of the source SBOM, please follow
[Software Bill-of-Materials (SBOM)](`e:system:sbom.md`)).

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

**Tip**
In cases where one needs to test the `.github/scripts/otp-compliance.es` script,
it is useful to download `scan-result.json` from any pull request in Erlang/OTP.
This file can be found in `Save ORT Scanner cache` step inside the `Create SBOM` Github task.
The scanning phase may take 2 h to run locally.
By downloading a recent scan, one can run generation of the SPDX report immediately,
and test the results running `.github/scripts/otp-compliance.es sbom otp-info --sbom-file ort/cli/bom.spdx.json --input-file ort/cli/scan-result.json`.


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
- `downloadLocation`: URI of the vendor library to download.
- `homepage`: homepage of the vendor library.
- `licenseDeclared`: license as declared by the vendor, following a [SPDX license identifier](https://spdx.org/licenses/).
- `name`: name of the library.
- `versionInfo`: version of the library/project/3pp. In case of no version number being available, write the commit sha.
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
When running the SBOM generator, make sure to check that the new vendor dependency exists
in its own package.

The [`renovate.json5`](../renovate.json5) file also needs to be updated
to make sure that the new vendored dependency gets updated as it should.

### Delete a Vendor Application

Delete the code and any remaining `vendor.info` files.
Re-run the source SBOM generation steps ([Erlang/OTP source SBOM]).

Delete the proper sections in [`renovate.json5`](../renovate.json5).

## VEX

VEX files allow to communicate which vulnerabilities are false positives and which ones are actual vulnerabilities. VEX files are important to explicitly state that some vendor dependencies are (not) vulnerabilities in your software.

Erlang/OTP has chosen to communicate VEX information using the OpenVEX implementation.

### Dependencies

Install `vexctl`, which is written in Go.

#### Installing Go

An easy way to install go on Ubuntu is to type the following:

```bash
sudo snap install go --classic
```

and add to your PATH the `snap` apps (e.g., to your `.bashrc`),

```bash
export PATH=$PATH:/snap/bin
```

Alternatively, install [Go from source or binaries](https://go.dev/doc/install).

#### Installing vexctl

**Automatic**

```
go install github.com/openvex/vexctl@latest
```

**From source**

Download and install `vextctl` as follows

```bash
git clone https://github.com/openvex/vexctl.git
cd vexctl
make
```

Add to your path the binary,

```bash
export PATH=$PATH:<your-path-to-vexctl>/vexctl/
```

### HOW-TO

Erlang/OTP will maintain VEX files for the latest three releases.
Because of this, Erlang/OTP will always contain the latest information in the `master` branch.
Any OpenVEX file in other branches is considered outdated.

The OpenVEX files are located in `vex/otp-26.openvex.json`, `vex/otp-27.openvex.json`, and `vex/otp-28.openvex.json` (e.g.). These files are generated from the `make/openvex.table` and the script `.github/scripts/otp-compliance.es`.

- `make/openvex.table` contains all known CVEs on a per release basis, with top-level objects for `otp-XX` branches, where each `otp-XX` object has as value a list of dependencies with their CVE and the status.

  Example:

  ```json
      "otp-28":
      [
          {
              "pkg:github/openssl/openssl@636dfadc70ce26f2473870570bfd9ec352806b1d" : "CVE-2025-4575",
              "status": {"not_affected": "vulnerable_code_not_present"}
          },

          {
              "pkg:github/PCRE2Project/pcre2@2dce7761b1831fd3f82a9c2bd5476259d945da4d": "OSV-2025-300",
              "status": {"not_affected": "vulnerable_code_not_present"}
          },
          ...
      ]
  ```

The `status` corresponds to the possible status from the [OpenVEX specification](https://github.com/openvex/spec/blob/main/OPENVEX-SPEC.md).
In case of `not_affected`, a reason must be provided (similar to the [specification](https://github.com/openvex/spec/blob/main/OPENVEX-SPEC.md)).
**The `make/openvex.table` is considered to be an append-only structure, where one should not do modifications to existing data nor removal**.
Changes should be done via `.github/scripts/otp-compliance.es` applied on the `openvex.table`. The main reason is to use
`openvex.table` as a simple source of truth without boilerplate, since VEX statements can be long due to the way in which one
must express range versions for a vulnerability.

In the example above, `pkg:github/openssl/openssl@636dfadc70ce26f2473870570bfd9ec352806b1d` corresponds to
the package URL for the OpenSSL version with commit `636dfadc70ce26f2473870570bfd9ec352806b1d`, which corresponds
to the version of OpenSSL used in OTP-X. Starting from OTP 28, this information can be found in the corresponding
`vendor.info` file for OpenSSL (e.g., `/erts/emulator/openssl/vendor.info` in the `sha` field).

### Further Format Details of openvex.table

The file `openvex.table` is a subset of fields of the OpenVEX specification.
The format is a JSON object that contains OTP VEX statements. A top-level valid field is the
OTP version key (e.g., `otp-29`), followed by a list of objects. Each JSON object can have the following structure.

- A key with a Purl, which uniquely identifies the application that the statement talks about,
  and a CVE string as value.
- A key `status` with value `Status :: "affected" | "fixed" | "under_investigation | "not_affected" | Affected`,
  where `Affected` is an object explained below.
- `Affected` is an object that may have the following keys
  - `affected` with value string that explains mitigation strategies
  - `fixed` with value where the fix was introduced.

**Example**

Assume the following ficticious case, where we want to report `CVE-2023-48795` on OTP 23.

```json
{
  "otp-23": [
    {
      "pkg:otp/ssh@4.10.1": "CVE-2023-48795",
      "status":
          { "affected": "Mitigation: If strict KEX availability cannot be ensured on both connection sides, affected encryption modes(CHACHA and CBC) can be disabled with standard ssh configuration. This will provide protection against vulnerability, but at a cost of affecting interoperability"
          }
    },
    {
      "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
      "status": { "affected": "Mitigation message, update to the next release"}
    }
  ]
}
```


### Use Cases

In all the cases explained below, the running of the tool `.github/scripts/otp-compliance.es vex` does not commit changes.
One has to execute the output commands to introduce changes.


**Code Generation**

Erlang/OTP creates releases (e.g., OTP-28) and also divides apps in different versions, which means that one can declare
vulnerabilities in multiple ways.

Example, should we mention that the `ssh` app in OTP-23 is vulnerable or that `ssh-4.10.1` which released in OTP-23 is vulnerable, or both?
To help us produce accurate results, Erlang/OTP favours to write the exact application version in which a bug was introduced or detected,
and the exact app version in which the app was fixed, within its OTP release.

For example, if we place in `openvex.table`:

```json
{
  "otp-23": [
    {
      "pkg:otp/ssh@4.10.1": "CVE-2023-48795",
      "status": { "affected": "Mitigation: If strict KEX availability cannot be ensured on both connection sides, affected encryption modes(CHACHA and CBC) can be disabled with standard ssh configuration. This will provide protection against vulnerability, but at a cost of affecting interoperability",
                  "fixed": ["pkg:otp/ssh@4.11.1.6"]
                }
    }
  ]
}
```

and execute the script

```bash
.github/scripts/otp-compliance.es vex run -b otp-23 | bash
```

Generates the following VEX commands

```bash
vexctl add --in-place vex/otp-23.openvex.json --product='pkg:otp/erlang@23.1,pkg:otp/erlang@23.1.1,pkg:otp/erlang@23.1.2,pkg:otp/erlang@23.1.3,pkg:otp/erlang@23.1.4,pkg:otp/erlang@23.1.5,pkg:otp/erlang@23.2,pkg:otp/erlang@23.2.1,pkg:otp/erlang@23.2.2,pkg:otp/erlang@23.2.3,pkg:otp/erlang@23.2.4,pkg:otp/erlang@23.2.5,pkg:otp/erlang@23.2.6,pkg:otp/erlang@23.2.7,pkg:otp/erlang@23.3,pkg:otp/erlang@23.3.1,pkg:otp/erlang@23.3.2,pkg:otp/erlang@23.3.3,pkg:otp/erlang@23.3.4,pkg:otp/erlang@23.3.4.1,pkg:otp/erlang@23.3.4.2,pkg:otp/erlang@23.3.4.3,pkg:otp/erlang@23.3.4.4,pkg:otp/erlang@23.3.4.5,pkg:otp/erlang@23.3.4.6,pkg:otp/erlang@23.3.4.7,pkg:otp/erlang@23.3.4.8,pkg:otp/erlang@23.3.4.9,pkg:otp/erlang@23.3.4.10,pkg:otp/erlang@23.3.4.11,pkg:otp/erlang@23.3.4.12,pkg:otp/erlang@23.3.4.13,pkg:otp/erlang@23.3.4.14,pkg:otp/ssh@4.10.1,pkg:otp/ssh@4.10.2,pkg:otp/ssh@4.10.3,pkg:otp/ssh@4.10.4,pkg:otp/ssh@4.10.5,pkg:otp/ssh@4.10.6,pkg:otp/ssh@4.10.7,pkg:otp/ssh@4.10.8,pkg:otp/ssh@4.11,pkg:otp/ssh@4.11.1,pkg:otp/ssh@4.11.1.1,pkg:otp/ssh@4.11.1.2,pkg:otp/ssh@4.11.1.3,pkg:otp/ssh@4.11.1.4,pkg:otp/ssh@4.11.1.5' --vuln='CVE-2023-48795' --status='affected' --action-statement='Mitigation: If strict KEX availability cannot be ensured on both connection sides, affected encryption modes(CHACHA and CBC) can be disabled with standard ssh configuration. This will provide protection against vulnerability, but at a cost of affecting interoperability'

vexctl add --in-place vex/otp-23.openvex.json --product='pkg:otp/erlang@23.3.4.19,pkg:otp/erlang@23.3.4.18,pkg:otp/erlang@23.3.4.17,pkg:otp/erlang@23.3.4.16,pkg:otp/erlang@23.3.4.15,pkg:otp/ssh@4.11.1.6' --vuln='CVE-2023-48795' --status='fixed'
```

The first command in the script has figured out the exact OTP versions that are vulnerable from the range of affected and fixed exact versions,
as well as created the range of `ssh` applications that are affected by the vulnerability.

The second command simply states which OTP application versions are fixed.

Below we continue with how to initialize and use the tool to report various states,
and show examples for Erlang/OTP applications and third party application on which Erlang/OTP builds upon.

#### Init

This will only be needed once, but if you need to initialize and provide existing known CVEs, you can use `.github/scripts/otp-compliance.es`.

The first time that we generate OpenVEX statements we call `.github/scripts/otp-compliance.es vex init --input-file make/openvex.table -b otp-28`. This init script outputs instructions to execute in the shell, which invokes commands from `vexctl` ([Installation steps here](https://github.com/openvex/vexctl)). You can run and execute the scripts as follows, `.github/scripts/otp-compliance.es vex init --input-file make/openvex.table -b otp-28 | bash` (if you use bash).

The script is idempotent, meaning that running consecutive times the script will not change its input.
Because of this, you run this command only for a new OTP release, and for coming CVEs you use `.github/scripts/otp-compliance.es vex run ...`.
This last command will not update the time and assumes that the `otp-XX.openvex.json` exists (because the `init` command must be run first).

**Example for new Release**

To release VEX files for a new release, OTP-29, add the name branch to `make/openvex.table` (assuming there are known CVEs):

```
{
    "otp-29": []
}
```

Execute the script to create the VEX statements for OTP-29:

```bash
.github/scripts/otp-compliance.es vex init --input-file make/openvex.table -b otp-29
```

There are no known vulnerabilities, so this VEX statement can be published as is.


#### Add `under_investigation`

For vendor CVEs, it may make sense to communicate with the ecosystem that a CVE for vendor X is under investigation.
If it is trivial to know whether we are affected, one could skip reporting `under_investigation` and add directly the `fixed`, or `vulnerable` statements.

To update or insert VEX statements for OTP-29, update the `make/openvex.table` and run:

```bash
.github/scripts/otp-compliance.es vex run --input-file make/openvex.table -b otp-29
```

The script will output commands to run (similar to a dry-run). Once piped to `bash`, they are executed.

```
.github/scripts/otp-compliance.es vex run --input-file make/openvex.table -b otp-29 | bash
```

Add and commit the changes.

**Example**

`make/openvex.table` contains:

```
{
    "otp-29": []
}
```

Lets assume there is `FIKA-2026-BROD` detected in `zlib`. We can issue an `under_investigation` statement updating the `make/openvex.table`


```json
{
    "otp-29": [
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": "under_investigation"
          }
    ]
}
```

Execute the command below to update the OpenVEX statements.

```bash
.github/scripts/otp-compliance.es vex run --input-file make/openvex.table -b otp-29 | bash
```

Erlang/OTP should not issue an `under_investigation` unless it is known that it will take some days to understand if Erlang/OTP is vulnerable to a vendor dependency.

#### Add `not_affected`

If the vulnerability under investigation is a false positive, one can convey this information using OpenVEX statements.
To do this, one adds a reason for why the vulnerability does not apply. These justifications can be found in the [OpenVEX spec](https://github.com/openvex/spec/blob/main/OPENVEX-SPEC.md#status-justifications).

**Example**

OTP was investigating the CVE `FIKA-2026-BROD` and found themselves not affected.
We continue from the example in the previous section, that contained `zlib` with status `under_investigation`:

```json
{
    "otp-29": [
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": "under_investigation"
          }
    ]
}
```

One can update the `make/openvex.table` with the reason of "code not present", meaning, the component is included
in OTP but the vulnerable code is not present. It is important to note that any statement written in the table
should not be updated, the table is append only.

```json
{
    "otp-29": [
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": "under_investigation"
          },
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": {"not_affected": "vulnerable_code_not_present"}
          }
    ]
}
```

To update the OpenVEX statements, run:

```bash
.github/scripts/otp-compliance.es vex run --input-file make/openvex.table -b otp-29 | bash
```

It produces a new entry in the openvex statements for OTP-29 stating that OTP-29 is not vulnerable to the CVE `FIKA-2026-BROD`.


#### Add `affected`

When OTP is affected by a CVE, one can communicate this using the `affected` status.

**Example**

OTP was investigating the CVE `FIKA-2026-BROD` and found themselves affected.

```json
{
    "otp-29": [
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": "under_investigation"
          }
    ]
}
```

One can write then in `make/openvex.table`:

```json
{
    "otp-29": [
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": "under_investigation"
          },
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": "affected"
          }
    ]
}
```

where the version affected is written as part of the package url `pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc`.

Execute the command below to update the OpenVEX statements.

```bash
.github/scripts/otp-compliance.es vex run --input-file make/openvex.table -b otp-29 | bash
```

It produces a new entry in the openvex statements for OTP-29.
One can run multiple times the same statement without introducing each time the same statement.
(the script makes the operation idempotent).

In some cases, it may be useful to provide additional information to mitigate the vulnerability.
To specify this, write the `status` value as an object with free text.

```json
{
    "otp-29": [
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": "under_investigation"
          },
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": {"affected" : "do not use this component until there is a fix"}
          }
    ]
}
```
 and run the `otp-compliance` script as stated above.

#### Add `fixed`

One can specify that the CVE is fixed in a specific version using the `fixed` keyword in the `make/openvex.table` statements.

**Example**

OTP was affected the CVE `FIKA-2026-BROD`, reported in `make/openvex.table`.

```json
{
    "otp-29": [
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": "under_investigation"
          },
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": "affected"
          }
    ]
}
```

OTP creates an emergency patch to fix this vendor dependency, and states that the package url (product and version)
`pkg:github/madler/zlib@04f42cecafika2026brod` fixes the vulnerability.

```json
{
    "otp-29": [
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": "under_investigation"
          },
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": "affected"
          },
          {
              "pkg:github/madler/zlib@04f42cecafika2026brod": "FIKA-2026-BROD",
              "status": "fix"
          },
    ]
}
```


Execute the command below to update the OpenVEX statements.

```bash
.github/scripts/otp-compliance.es vex run --input-file make/openvex.table -b otp-29 | bash
```

Alternatively, one can write the affected and fixed versions in a single object for OTP applications.

```json
    "otp-29": [
          {
              "pkg:otp/erts@10.3.4": "FIKA-2026-BROD",
              "status": "under_investigation"
          },
          {
              "pkg:otp/erts@10.3.4": "FIKA-2026-BROD",
              "status": { "affected": "Mitigation message, update to the next release",
                          "fixed": ["pkg:otp/erts@10.3.20"]}
          }
    ]
```

#### Vendor Statements

Some vendor applications may be tied to the runtime system, such as `openssl` is tied to `erts` and `erl_interface`.
When there is a CVE towards a third party tied to an Erlang/OTP package (almost always!),
where Erlang/OTP is not affected (almost all cases of `openssl`), one can write the following,
where `apps` is a list of applications not affected, started from their package url version
(`14.2.5.10`) until implicitly their last version in the tree.

```json
    {
      "pkg:github/openssl/openssl@0foobar": "CVE-2024-9143",
      "status": { "not_affected": "vulnerable_code_not_present",
                  "apps": ["pkg:otp/erts@14.2.5.10"]}
    },
```


In case of wanting to explicitly state that Erlang/OTP is vulnerable or not to a vendor CVE,
one can write the following, but the script will not generate range queries due to these
been at the sha-1 commit hash level.

```json
{
    "otp-29": [
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": "under_investigation"
          },
          {
              "pkg:github/madler/zlib@04f42ceca40f73e2978b50e93806c2a18c1281fc": "FIKA-2026-BROD",
              "status": { "affected": "Mitigation message, update to the next release",
                          "fixed": ["pkg:github/madler/zlib@04f42thiscommitfixesthecve"]}
          }
    ]
}
```
