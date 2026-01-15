#!/bin/bash

## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0
##
## Copyright Ericsson AB 2020-2025. All Rights Reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
## %CopyrightEnd%

## This bash script takes 4 arguments
##  1: Repository on github to sync to
##  2: The auth token to be used (either "token TheToken" or "Bearer TheToken")
##  3: A regexp for which releases to sync
##  4: Optional timeout for the rsync command when it should stop syncing
##
## This script downloads artifacts from erlang.org/download and then publishes
## them to the release for the corresponding TAG on github. If there is no release
## then a release is created with the README as the body of the release.
##
## The script does not keep release artifacts up to date, so if an artifact is changed
## on erlang.org then it will not be automatically updated on github.
##
## The reason why this is a polling script and not triggered when a new tag is pushed
## is because when the TAG is pushed to github it is not guarenteed that the
## corresponding artifacts will be available on erlang.org.

set -e

REPOSITORY=${1}
TOKEN=${2:-"token ${GITHUB_TOKEN}"}
RELEASE_FILTER=${3:-"^[2-9][1-9]\\..*"}
TIME_LIMIT=${4:-120m}
HDR=(-H "Authorization: ${TOKEN}")
REPO="https://api.github.com/repos/${REPOSITORY}"

_json_escape () {
    echo "${1}" | python -c 'import json,sys; print(json.dumps(sys.stdin.read()))'
}

_strip_name() {
    echo "${1}" | sed -e 's/^OTP[-_]//g'
}

_curl_get() {
    curl --silent "${HDR[@]}" "${@}"
}

_curl_post() {
    curl -o /dev/null --silent --fail --show-error -X POST "${HDR[@]}" \
         -H "Accept: application/vnd.github.v3+json" "${@}"
}

_curl_patch() {
    curl -o /dev/null --silent --fail --show-error -X PATCH "${HDR[@]}" \
         -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" \
        "${@}"
}

_format_markdown() {
    TMP_BODY=$(mktemp)
    echo "${1}" > "${TMP_BODY}"
    mdformat --wrap no "${TMP_BODY}"
    cat "${TMP_BODY}"
    rm -rf "${TMP_BODY}"
}

RI=()
ALL_TAGS=()
CREATE_RELEASE=()
UPDATE_BODY=()
TAG_URL="${REPO}/tags?per_page=100"

## This function is used to loop over the pagianated results from github tags
## It sets TAGS to be the json from the current page of tags
_next_page() {
    TAGS=$(curl -s "${HDR[@]}" ${TAG_URL})
    ## In the "Link:" header from github we get the link for the next page.
    ## An example link header:
    ##   link: <https://api.github.com/repositories/843890/tags?per_page=100&page=2>; rel="next", <https://api.github.com/repositories/843890/tags?per_page=100&page=4>; rel="last"
    TAG_URL=$(curl -s -I -H "${HDR[@]}" ${TAG_URL} | grep "^link:" | sed -n 's/link:.* <\([^>]\+\)>; rel="next".*/\1/p')
}

## First we fetch all tags and releases and build a list of all resources
## that we should rsync from erlang.org. We only want to do one call to
## rsync for all files as otherwise erlang.org will rate-limit us.
while [ "${TAG_URL}" != "" ]; do
    _next_page

    ## Loop over all tags, we base64 encode each element in the array
    ## in order to make the bash for loop work
    for row in $(echo "${TAGS}" | jq -r '.[] | @base64'); do
        _row() {
            echo "${row}" | base64 --decode | jq -r "${1}"
        }
        name=$(_row '.name')
        stripped_name=$(_strip_name ${name})
        RELEASE_VSN=$(echo "${stripped_name}" | awk -F. '{print $1}')

        if echo ${stripped_name} | grep -E "${RELEASE_FILTER}" > /dev/null; then
            RELEASE=$(_curl_get "${REPO}/releases/tags/${name}")
            if ! echo "${RELEASE}" | jq -er ".name" > /dev/null; then
                ALL_TAGS=("${ALL_TAGS[@]}" "${name}")
                CREATE_RELEASE=("${CREATE_RELEASE[@]}" "${name}")
                RI=("*${stripped_name}*" "${RI[@]}")
                echo "Create release ${name}"
            else
                _asset() {
                    local filename="${1}"
                    local remotename=("${2:-$filename}")
                    if [ $# -gt 2 ]; then
                        shift; shift
                        remotename=("$@" "${remotename[@]}")
                    fi
                    if ! echo "${RELEASE}" | jq -er ".assets[] | select(.name == \"${filename}\")" > /dev/null; then
                        ALL_TAGS=("${ALL_TAGS[@]}" "${name}")
                        echo "Sync ${remotename[*]} for ${name}"
                        RI=("${remotename[@]}" "${RI[@]}")
                    fi
                }

                ## Check if we need to patch the body of the release
                RELEASE_BODY=$(echo "${RELEASE}" | jq --raw-output '.body' | tr -d '\015')
                UPDATE_RELEASE_BODY=yes
                if [ "${RELEASE_BODY}" != "" ]; then
                    ## Check if we need to reformat the body
                    FORMATTED_BODY=$(_format_markdown "${RELEASE_BODY}")
                    if [ "${FORMATTED_BODY}" = "${RELEASE_BODY}" ]; then
                        UPDATE_RELEASE_BODY=no
                    else
                        UPDATE_RELEASE_BODY=format
                    fi
                    rm -rf "${TMP_BODY}"
                fi
                if [ "${UPDATE_RELEASE_BODY}" != "no" ]; then
                    RELEASE_ID=$(echo "${RELEASE}" | jq '.id')
                    UPDATE_BODY=("${UPDATE_BODY[@]}" "${name}:${RELEASE_ID}")
                    if [ "${UPDATE_RELEASE_BODY}" = "yes" ]; then
                        if [[ ${RELEASE_VSN} -gt 26 ]]; then
                            RM="${name}.README.md"
                        else
                            RM="${name}.README"
                        fi
                        echo "Sync ${RM} for ${name} (for update of release body, release id = ${RELEASE_ID})"
                        RI=("${RM}" "${RI[@]}")
                    fi
                fi

                _asset "${name}.README" "${name}.README" "otp_src_${stripped_name}.readme"
                if [[ ${RELEASE_VSN} -gt 26 ]]; then
                    _asset "${name}.README.md"
                fi
                _asset "otp_src_${stripped_name}.tar.gz"
                _asset "otp_doc_html_${stripped_name}.tar.gz"
                case "${stripped_name}" in
                    27.0**)
                    ## There are no man pages for 27.0 release
                    ;;
                    *)
                        _asset "otp_doc_man_${stripped_name}.tar.gz"
                        ;;
                esac
                case "${stripped_name}" in
                    22.*.**|21.*.**)
                    ## No need to check for windows releases in 21 and 22 patches
                    ;;
                    *)
                        _asset "otp_win32_${stripped_name}.exe"
                        _asset "otp_win64_${stripped_name}.exe"
                        if echo "${RELEASE}" | jq -er ".assets[] | select(.name == \"otp_win32_${stripped_name}.exe\")" > /dev/null; then
                            _asset "otp_win32_${stripped_name}.zip"
                        fi
                        if echo "${RELEASE}" | jq -er ".assets[] | select(.name == \"otp_win64_${stripped_name}.exe\")" > /dev/null; then
                            _asset "otp_win64_${stripped_name}.zip"
                        fi
                        ;;
                esac
            fi
        fi
    done
done

## Remove all duplicate tags
readarray -t ALL_TAGS < <(printf '%s\0' "${ALL_TAGS[@]}" | sort -uz | xargs -0n1)

RINCLUDE=()
for i in "${RI[@]}"; do
    RINCLUDE=("--include=${i}" "${RINCLUDE[@]}")
done

set -x
## rsync the proper files, we will use which files have been
## synced to determine which artifacts we should upload.
## There is a timelimit here so that github actions will not
## timeout
! timeout ${TIME_LIMIT} rsync --archive --verbose --compress "${RINCLUDE[@]}" --exclude='*' \
  erlang.org::erlang-download downloads
set +x

## Rename all .readme files to .README
for name in "${ALL_TAGS[@]}"; do
    stripped_name=$(_strip_name ${name})
    if [ -s "downloads/otp_src_${stripped_name}.readme" ]; then
        mv downloads/otp_src_${stripped_name}.readme downloads/${name}.README
    fi
done

## All tags that do not have a release we create a release for
## using the readme as the body text if a readme is available.
for name in "${CREATE_RELEASE[@]}"; do
    echo "Create release for ${name}"
    stripped_name=$(_strip_name ${name})
    if [ -s "downloads/${name}.README.md" ]; then
        mdformat --wrap no "downloads/${name}.README.md"
        README=$(cat downloads/${name}.README.md)
        README=$(_json_escape "${README}")
    elif [ -s "downloads/${name}.README" ]; then
        README=$(cat downloads/${name}.README)
        if echo "${README}" | grep "HIGHLIGHTS" > /dev/null; then
            ## We have highlights, so only use those as the body

            ## This awk script is a hack.
            ## It counts the number of lines that start with '---' and
            ## then outputs any text after the first '---' until the 7th.
            README=$(echo "${README}" | awk 'BEGIN{ echo=0 } { if ( $1 ~ /^---/ ) { echo++ } if ( echo > 0 && echo < 7 ) { print $0 } }')
        fi
        README=$(_json_escape "$(printf '```\n%s\n```' "${README}")")
    else
        README=""
    fi
    if [ "${README}" != "" ]; then
        BODY=", \"body\":${README}"
    else
        BODY=""
    fi
    TMP=$(mktemp)
    printf '{"tag_name":"'"%s"'", "name":"OTP '"%s\"%s}" "${name}" "${stripped_name}" "${BODY}" > "${TMP}"
    _curl_post "${REPO}/releases" --data-binary "@${TMP}"
    rm -f ${TMP}
done

for name_id in "${UPDATE_BODY[@]}"; do
    name=$(echo "${name_id}" | awk -F: '{print $1}')
    RELEASE_ID=$(echo "${name_id}" | awk -F: '{print $2}')
    if [ -s downloads/"${name}.README.md" ]; then
        mdformat --wrap no "downloads/${name}.README.md"
        README=$(cat downloads/"${name}.README.md")
        README=$(_json_escape "${README}")
    elif [ -s downloads/"${name}.README" ]; then
        README=$(cat downloads/"${name}.README")
        README=$(_json_escape "$(printf '```\n%s\n```' "${README}")")
    else
        ## This happens when we should reformat an already existing description
        RELEASE=$(_curl_get "${REPO}/releases/tags/${name}")
        README=$(echo "${RELEASE}" | jq --raw-output '.body' | tr -d '\015')
        README=$(_format_markdown "${README}")
        README=$(_json_escape "${README}")
    fi
    echo "Update body of ${name}"
    TMP=$(mktemp)
    printf "{\"body\":%s}" "${README}" > "${TMP}"
    _curl_patch "${REPO}/releases/${RELEASE_ID}" --data-binary "@${TMP}"
    rm -f ${TMP}
done

UPLOADED=false

## Array of all tags that do not have a pre-build
MISSING_PREBUILD=()

## Array of zip files that have been triggered this sync
MISSING_WIN_ZIP=()

_upload_artifacts() {
    local name=${1};
    local stripped_name=""
    local RELEASE=""
    local UPLOAD_URL=""
    echo "Upload artifacts for ${name}"
    stripped_name=$(_strip_name ${name})
    RELEASE=$(_curl_get "${REPO}/releases/tags/${name}")
    UPLOAD_URL=$(echo "${RELEASE}" | jq -r ".upload_url" | sed 's/{.*//')
    _upload() {
        if [ -s downloads/${1} ]; then
            ARTIFACT_ID=$(echo "${RELEASE}" | jq -r '.assets.[] | select(.name == "'"${1}"'")')
            if [ "${ARTIFACT_ID}" = "" ]; then
                echo "Upload ${1}"
                UPLOADED=true
                _curl_post -H "Content-Type: ${2}" \
                           "${UPLOAD_URL}?name=${1}" \
                           --data-binary "@downloads/${1}"
            else
                echo "Skipped upload of ${1}, it already exists"
            fi
        else
            ## See if we need to trigger any .exe to .zip convertions
            if echo "${RI[@]}" | grep "otp_${2}_${stripped_name}.zip" > /dev/null; then
                if [ ${#MISSING_WIN_ZIP[@]} -lt 20 ]; then
                    MISSING_WIN_ZIP=("${MISSING_WIN_ZIP[@]}" "${stripped_name}")
                    _curl_post "${REPO}/actions/workflows/upload-windows-zip.yaml/dispatches" \
                        -d '{"ref":"master", "inputs":{"version":"'"${stripped_name}"'", "target":"'"${2}"'"}}'
                fi
            fi
            ## See if we need to re-build any prebuilds
            if echo "${RI[@]}" | grep "${stripped_name}.tar.gz" > /dev/null; then
                MISSING_PREBUILD=("${MISSING_PREBUILD[@]}" "${name}")
            fi
        fi
    }
    _upload "${name}.README" "text"
    _upload "${name}.README.md" "text"
    _upload "otp_src_${stripped_name}.tar.gz" "application/gzip"
    _upload "otp_doc_html_${stripped_name}.tar.gz" "application/gzip"
    _upload "otp_doc_man_${stripped_name}.tar.gz" "application/gzip"
    _upload "otp_win32_${stripped_name}.exe" "application/x-msdownload"
    _upload "otp_win64_${stripped_name}.exe" "application/x-msdownload"
    _upload "otp_win32_${stripped_name}.zip" "win32"
    _upload "otp_win64_${stripped_name}.zip" "win64"
}

## Upload all assets for tags
for name in "${ALL_TAGS[@]}"; do
    _upload_artifacts ${name}
done

## Rebuild erlang.org to get links to the new artifacts
if [ ${UPLOADED} = true ]; then
    curl -H "Authorization: token ${ERLANG_ORG_TOKEN}" -X POST -H "Accept: application/vnd.github.v3+json" "https://api.github.com/repos/erlang/erlang-org/actions/workflows/update-gh-cache.yaml/dispatches" -d '{"ref":"master"}'
fi

## If no assets were uploaded, we try to build one instead
if [ ${UPLOADED} = false ]; then
    for name in "${MISSING_PREBUILD[@]}"; do
        stripped_name=$(_strip_name "${name}")
        release=$(echo "${stripped_name}" | awk -F. '{print $1}')
        if [[ $release -lt 24 ]]; then
            ## Releases before 24 are no longer supported and are a bit different
            ## from 24+ so I've removed support for them
            echo "Skipping old release ${name}"
            continue;
        fi
        echo "Building pre-build and docs for ${name}"
        git clone https://github.com/erlang/otp -b "${name}" otp_src
        if [ -f otp_src/.github/scripts/init-pre-release.sh ]; then
            (cd otp_src && ERL_TOP=$(pwd) .github/scripts/init-pre-release.sh)
        else
            (cd otp_src && ERL_TOP=$(pwd) ../.github/scripts/init-pre-release.sh)
        fi
        (cd otp_src && BASE_USE_CACHE=false GITHUB_OUTPUT=.tmp ../.github/scripts/build-base-image.sh maint-${release} 64-bit)
        docker build -t otp --build-arg ARCHIVE=otp_src/otp_src.tar.gz \
               -f otp_src/.github/dockerfiles/Dockerfile.64-bit .
        docker run -v "$PWD":/github otp \
               "/github/scripts/build-otp-tar -o /github/otp_clean_src.tar.gz /github/otp_src.tar.gz -b /buildroot/otp/ /buildroot/otp.tar.gz"
        .github/scripts/release-docs.sh "${release}" "${stripped_name}"
        .github/scripts/create-artifacts.sh downloads "${name}"

        ## Delete any artifacts that we should not upload
        for artifact in dowloads/*; do
            if ! echo "${RI[@]}" | grep "${artifact#downloads/}" 2> /dev/null > /dev/null; then
                rm -f "${artifact}"
            fi
        done
        _upload_artifacts "${name}"

        ## We only update one release per call to sync-github-releases
        break
    done
fi
