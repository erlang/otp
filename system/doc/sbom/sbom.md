<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2025. All Rights Reserved.

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

# Software Bill Of Materials

[](){: #sbom }

# Software Bill-of-Materials (SBOM)

A Software Bill-of-Materials (SBOM) is a document to share information about the
software used, dependencies and, essentially, what the software is made of. SBOMs have many
different use cases, and some examples include verification of licenses, or
vulnerability scanning using databases such as [CVE](https://www.cve.org/) and [OSV](https://osv.dev/), among others.

Erlang/OTP has multiple third-party dependencies. Some are vendored into the source code of Erlang/OTP:
- pcre2 (`erts/emulator/pcre`)
- zlib (`erts/emulator/zlib`)
- asmjit (`erts/emulator/asmjit`)
- openssl (`erts/emulator/openssl`)
- zstd (`erts/emulator/zstd`)
- others

The Erlang/OTP project provides source SBOMs starting with OTP 28. Below we detail structure of the source SBOM.

## Source SBOM Structure And General Understanding

Erlang/OTP publishes a source SBOM for Erlang/OTP using [SPDX v2.2](https://spdx.github.io/spdx-spec/v2.3/relationships-between-SPDX-elements/) format.
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
  We use `stdlib` as a running example to explain the package structure in the SPDX SBOM, where Erlang/OTP applications:
  - `SPDXRef-otp-stdlib-documentation` contains all documentation about `stdlib`, and
  - `SPDXRef-otp-stdlib-test` contains all tests about `stdlib`, and `SPDXRef-otp-stdlib` contains the source code of the `stdlib` application.

  ```json
  {
      "SPDXID": "SPDXRef-otp-stdlib",                                   <------- stdlib PACKAGE
      "downloadLocation": "https://github.com/erlang/otp/releases",
      "externalRefs": [...],
      "filesAnalyzed": true,
      "hasFiles": [
        "SPDXRef-File-9022",
        "SPDXRef-File-9023",
        ...
      ],
      "name": "stdlib",
      "packageVerificationCode": { "packageVerificationCodeValue": "29200c1cd7da4a5c015cdafd6f71db538ae0a1c9" },
      "supplier": "Organization: Ericsson AB",
      "versionInfo": "7.0.2"
  },
  {
      "SPDXID": "SPDXRef-otp-stdlib-documentation",                         <------- stdlib DOCUMENTATION PACKAGE
      ...
      "name": "stdlib-documentation",
      "packageVerificationCode": { "packageVerificationCodeValue": "ad443de0ca77bf6cbadc35813e0807494949f25c" },
      "supplier": "Organization: Ericsson AB",
      "versionInfo": "7.0.2"
  },
  {
      "SPDXID": "SPDXRef-otp-stdlib-test",                                   <------- stdlib TEST PACKAGE
      ...
  }
  ```

- Application packages have the following fields:
  - `name` which represents the Erlang/OTP application name, e.g., `stdlib`, `erts`, etc,
    and/or the application name with the suffix `documentation` or `test`, e.g., `stdlib-test` and `stdlib-documentation`.
  - `copyrightText` includes the copyright of all the files under the given package.
  - `downloadLocation` specifies where the package can be downloaded from.
  - `versionInfo` specifies the version of the application, which in case of documentation or test
     packages, it refers to the top-level application. For example, the `stdlib` package has `versionInfo` equals to `7.0.2` and its corresponding `stdlib-documentation` and `stdlib-test` packages will have the same `versionInfo`, as this is the version of the package.
  - `licenseInfoFromFiles` contains the list of licenses found in the files that belong to the given package.
  - for other clarications, please check the SPDX 2.2 standard.

- The application package, application test package, and the application documentation package may all in turn contain one or more vendor packages. An example of this is the package `SPDXRef-otp-erts` who contains other packages, such as `SPDXRef-otp-erts-asmjit`.

- To remove non-needed applications from your SBOM, remove the first level packages (Erlang/OTP applications) that are not needed, including all of their transitive dependencies (other packages reachable from them), as well as all files reachable from these packages. For example, to remove the application `ftp`, one must remove the package `SPDXRef-otp-ftp`, `SPDXRef-otp-ftp-documentation`, and `SPDXRef-otp-ftp-test`, and all the files that they reference (including also [relationship items](https://spdx.github.io/spdx-spec/v2.3/relationships-between-SPDX-elements/)). In most ocassions, you may want to remove the first level Erlang/OTP applications and the keep first level vendor dependencies (identified by comment "vendor package" in the SPDX package). The reason for keeping the first level vendor dependencies is that those include Erlang/OTP building scripts.

Below we show how the `stdlib` packages are linked between them and against the root package, `"SPDXRef-Project-OTP"`.
In this particular case, `stdlib` does not have any more relationships. But Erlang/OTP applications have dependencies
in their `app.src` file and these are also captured in the source SBOM in the relationships field. If you remove packages,
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
         "relatedSpdxElement": "SPDXRef-otp-stdlib",
         "relationshipType": "TEST_OF",                 <----- THESE ARE TESTS
         "spdxElementId": "SPDXRef-otp-stdlib-test"
       },
       {
         "relatedSpdxElement": "SPDXRef-otp-stdlib",
         "relationshipType": "DOCUMENTATION_OF",        <----- THESE ARE DOCUMENTS, EXAMPLES, ETC
         "spdxElementId": "SPDXRef-otp-stdlib-documentation"
       },
       {
         "relatedSpdxElement": "SPDXRef-Project-OTP",
         "relationshipType": "PACKAGE_OF",              <------ THIS SPECIFIES THAT stdlib IS PART OF PROJECT-OTP
         "spdxElementId": "SPDXRef-otp-stdlib"
       },
       ...
    ]
  }
  ```


## Verification Of Source SBOM

In each release, Erlang/OTP releases a source SBOM together with a signed SBOM attestation artifact.
This gives users the ability to verify the signed artefact.

Below we show how to do this for Erlang/OTP version `28.0.2` using Sigstore `cosign` ([installation](https://github.com/sigstore/cosign)) and/or Github `gh` tools ([installation](https://github.com/cli/cli)).

### Sigstore `cosign`

1. Download the SBOM for `28.0.2`, named `bom.spdx.json` ([here](https://github.com/erlang/otp/releases/download/OTP-28.0.2/bom.spdx.json))
2. Download the sigstore file, `bom.spdx.json.sigstore` ([here](https://github.com/erlang/otp/releases/download/OTP-28.0.2/bom.spdx.json.sigstore))
3. Run `cosign` with the following parameters

   ```bash
   cosign verify-blob-attestation \
      --bundle "bom.spdx.json.sigstore" \
      --new-bundle-format \
      --type "https://spdx.dev/Document/v2.2" \
      --certificate-oidc-issuer "https://token.actions.githubusercontent.com" \
      --certificate-identity "https://github.com/erlang/otp/.github/workflows/main.yaml@refs/tags/OTP-28.0.2" \
      "bom.spdx.json"
   ```
### Github CLI `gh`

1. Download the SBOM for `28.0.2`, named `bom.spdx.json` ([here](https://github.com/erlang/otp/releases/download/OTP-28.0.2/bom.spdx.json))
2. Run `gh` with the following parameters

   ```bash
   gh attestation verify \
      --predicate-type "https://spdx.dev/Document/v2.2" \
      --repo "erlang/otp" \
      --source-ref "refs/tags/OTP-28.0.2" \
      "bom.spdx.json"
   ```
