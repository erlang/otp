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
structure of the source SBOM and the steps necessary to run yourself the generation of the Erlang/OTP source SBOM.

## Source SBOM Structure And General Understanding

Erlang/OTP publishes a source SBOM for Erlang/OTP using [SPDX v2.2]( https://spdx.github.io/spdx-spec/v2.3/relationships-between-SPDX-elements/) format.
The source SBOM can be seen as a tree data structure.

- `root`: the root of the tree is found under the key `documentDescribes`.
  The value is a single item that points to the SPDX `package` that represents the root node from where all packages converge.
  This root node contains mostly configuration files that do not belong to Erlang/OTP applications nor runtime applications.
  All SPDX packages (Erlang/OTP apps and runtime, explained later) are under the `packages` key in the SPDX document.

  ```json
  {
    "SPDXID": "SPDXRef-DOCUMENT",
    "creationInfo": { ... },
    "dataLicense": "CC0-1.0",
    "documentDescribes": [ "SPDXRef-Project-OTP" ],     <----- ROOT NODE
    ...,

    "name": "Erlang/OTP",
      "packages": [
        {
          "SPDXID": "SPDXRef-Project-OTP",              <----- DESCRIPTION OF ROOT NODE
          "downloadLocation": "https://github.com/erlang/otp/releases",
          "externalRefs": [ { "comment": "",
                              "referenceCategory": "PACKAGE-MANAGER",
                              "referenceLocator": "pkg:github/erlang/otp@28.0.1",
                              "referenceType": "purl"
                            } ],
        "filesAnalyzed": true,
        "hasFiles": [ "SPDXRef-File-1", "SPDXRef-File-2", ...]  <----- FILES IN ROOT NODE
        },
        ...      <----- OTHER PACKAGES LIKE ERTS
      ]
  }
  ```

- First level branches from `root` represent Erlang/OTP applications, the runtime system (`erts`), and
  some vendor build scripts (`SPDXRef-otp-make-install-sh`). As an example, we show below the `erts` package.

  ```json
  {
    "SPDXID": "SPDXRef-otp-erts",
    "downloadLocation": "https://github.com/erlang/otp/releases",
    "externalRefs": [ { "comment": "Erlang Runtime System",
                        "referenceCategory": "PACKAGE-MANAGER",
                        "referenceLocator": "pkg:otp/erts@16.0.1?vcs_url=git+https://github.com/erlang/otp.git",
                        "referenceType": "purl"}],
    ...
    "filesAnalyzed": true,
    "hasFiles": [ "SPDXRef-File-380", ...],
    "name": "erts",
    "packageVerificationCode": { "packageVerificationCodeValue": "2568c51ee8756f36b6173037035ca4f77ed0d00b" },
    "supplier": "Organization: Ericsson AB",
    "versionInfo": "16.0.1"
  },
  ```

- All Erlang/OTP application SPDX packages are named with the prefix
  `SPDXRef-otp-<appname>`. `<appname>` represents the name of an Erlang
  application, where the value is the name of the Erlang application with the
  underscores `_` dropped, e.g., `common_test` becomes `commontest`.
  
- Application packages have at least two sub packages. One for tests and one for docs.   

  The documentation and the tests packages add a suffix to the `SPDXRef-otp-<appname>`, namely `documentation` and `test`.
  We use `wx` as a running example to explain the package structure in the SPDX SBOM, where Erlang/OTP applications:
  - `SPDXRef-otp-wx-documentation` contains all documentation about `wx`, and
  - `SPDXRef-otp-wx-test` contains all tests about `wx`, and `SPDXRef-otp-wx` contains the source code of the `wx` application.

  ```json
  {
      "SPDXID": "SPDXRef-otp-wx",                                   <------- WX PACKAGE
      "downloadLocation": "https://github.com/erlang/otp/releases",
      "externalRefs": [ ... ],
      "filesAnalyzed": true,
      "hasFiles": [
        "SPDXRef-File-10715",
        "SPDXRef-File-10716",
      "name": "wx",
      "packageVerificationCode": { "packageVerificationCodeValue": "bf245bf9f04a6a72a6aa1a8ffed24a1caea578df" },
      "supplier": "Organization: Ericsson AB",
      "versionInfo": "2.5"
  },
  {
      "SPDXID": "SPDXRef-otp-wx-documentation",                         <------- WX DOCUMENTATION PACKAGE
      ...
      "name": "wx-documentation",
      "packageVerificationCode": { "packageVerificationCodeValue": "17ca54aba897f07f490b729f7f11a686092cd736" },
      "supplier": "Organization: Ericsson AB",
      "versionInfo": "2.5"
  },
  {
      "SPDXID": "SPDXRef-otp-wx-test",                                   <------- WX TEST PACKAGE
      "copyrightText": "Copyright Ericsson AB 2010-2025. All Rights Reserved.\nCopyright Ericsson AB 2011-2025. All Rights Reserved.\nCopyright Ericsson AB 2017-2025. All Rights Reserved.\nNOASSERTION\nCopyright Ericsson AB 2009-2025. All Rights Reserved.\nCopyright Ericsson AB 2008-2025. All Rights Reserved.\n",
      "downloadLocation": "https://github.com/erlang/otp/releases",
      "externalRefs": [],
      "filesAnalyzed": true,
      ...
  }
  ```

- Application packages have the following fields:
  - `name` which represents the Erlang/OTP application name, e.g., `common_test`, `erts`, etc,
    and/or the application name with the suffix `documentation` or `test`, e.g., `common_test-test` and `common_test-documentation`.
  - `copyrightText` includes the copyright of all the files under the given package.
  - `downloadLocation` specifies where the package can be downloaded from.
  - `versionInfo` specifies the version of the application, which in case of documentation or test
     packages, it refers to the top-level application. For example, the `wx` package has `versionInfo` equals to `2.5.1` and its corresponding `wx-documentation` and `wx-test` packages will have the same `versionInfo`, as this is the version of the package.
  - `licenseInfoFromFiles` contains the list of licenses found in the files that belong to the given package.
  - for other clarications, please check the SPDX 2.2 standard.

- The application package, application test package, and the application documentation package may all in turn contain one or more vendor packages. An example of this is the package `SPDXRef-otp-erts` who contains other packages, such as `SPDXRef-otp-erts-asmjit`.

- To remove non-needed applications from your SBOM, remove the first level packages (Erlang/OTP applications) that are not needed, including all of their transitive dependencies (other packages reachable from them), as well as all files reachable from these packages. For example, to remove the application `wx`, one must remove the package `SPDXRef-otp-wx`, `SPDXRef-otp-wx-documentation`, and `SPDXRef-otp-wx-test`, and all the files that they reference (including also [relationship items](https://spdx.github.io/spdx-spec/v2.3/relationships-between-SPDX-elements/)). In most ocassions, you may want to remove first level Erlang/OTP applications and keep first level vendor dependencies (identified by comment "vendor package" in the SPDX package). The reason for keeping first level vendor dependencies is that those include Erlang/OTP building scripts.

  Below we show how the `wx` packages are linked between them and against the root package, `"SPDXRef-Project-OTP"`.
  In this particular case, `wx` does not have any more relationships. But Erlang/OTP applications have dependencies
  in their app.src file and these are also captured in the source SBOM in the relationships field. If you remove packages,
  you need to remove relationships that do not exist anymore.

  ```json
  {
    "SPDXID": "SPDXRef-DOCUMENT",
    "creationInfo": { ... },
    "dataLicense": "CC0-1.0",
    "documentDescribes": [ "SPDXRef-Project-OTP" ],     <----- ROOT NODE
    ...,

    "name": "Erlang/OTP",
    "packages": [ ... ],
    "relationships": [                                  <----- RELATIONSHIPS, OR, HOW EVERYTHING FITS TOGETHER
       {
         "relatedSpdxElement": "SPDXRef-otp-wx",
         "relationshipType": "TEST_OF",                 <----- THESE ARE TESTS
         "spdxElementId": "SPDXRef-otp-wx-test"
       },
       {
         "relatedSpdxElement": "SPDXRef-otp-wx",
         "relationshipType": "DOCUMENTATION_OF",        <----- THESE ARE DOCUMENTS, EXAMPLES, ETC
         "spdxElementId": "SPDXRef-otp-wx-documentation"
       },
       {
         "relatedSpdxElement": "SPDXRef-Project-OTP",
         "relationshipType": "PACKAGE_OF",              <------ THIS SPECIFIES THAT WX IS PART OF PROJECT-OTP
         "spdxElementId": "SPDXRef-otp-wx"
       },
       ...
    ]
  }
  ```

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
