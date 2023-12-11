#!/bin/bash

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
RELEASE_FILTER=${3}
TIME_LIMIT=${4:-120m}
HDR=(-H "Authorization: ${TOKEN}")
REPO="https://api.github.com/repos/${REPOSITORY}"

_json_escape () {
    printf '```\n%s\n```' "${1}" | python -c 'import json,sys; print(json.dumps(sys.stdin.read()))'
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

RI=()
ALL_TAGS=()
CREATE_RELEASE=()
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
                _asset "${name}.README" "${name}.README" "otp_src_${stripped_name}.readme"
                _asset "otp_src_${stripped_name}.tar.gz"
                _asset "otp_doc_html_${stripped_name}.tar.gz"
                _asset "otp_doc_man_${stripped_name}.tar.gz"
                case "${stripped_name}" in
                    22.*.**|21.*.**)
                    ## No need to check for windows releases in 21 and 22 patches
                    ;;
                    *)
                        _asset "otp_win32_${stripped_name}.exe"
                        _asset "otp_win64_${stripped_name}.exe"
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
    if [ -s "downloads/${name}.README" ]; then
        README=`cat downloads/${name}.README`
    else
        README=""
    fi
    if echo "${README}" | grep "HIGHLIGHTS" > /dev/null; then
        ## We have highlights, so only use those as the body

        ## This awk script is a hack.
        ## It counts the number of lines that start with '---' and
        ## then outputs any text after the first '---' until the 7th.
        README=`echo "${README}" | awk 'BEGIN{ echo=0 } { if ( $1 ~ /^---/ ) { echo++ } if ( echo > 0 && echo < 7 ) { print $0 } }'`
    fi
    if [ "${README}" != "" ]; then
        RM=$(_json_escape "${README}")
        BODY=", \"body\":${RM}"
    else
        BODY=""
    fi
    $(_curl_post "${REPO}/releases" -d '{"tag_name":"'"${name}"'", "name":"OTP '"${stripped_name}\"${BODY}}")
done


UPLOADED=false

## Array of all tags that do not have a pre-build
MISSING_PREBUILD=()

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
            echo "Upload ${1}"
            UPLOADED=true
            _curl_post -H "Content-Type: ${2}" \
                       "${UPLOAD_URL}?name=${1}" \
                       --data-binary "@downloads/${1}"
        else
            if echo "${RI[@]}" | grep "${stripped_name}.tar.gz" > /dev/null; then
                MISSING_PREBUILD=("${MISSING_PREBUILD[@]}" "${name}")
            fi
        fi
    }
    _upload "${name}.README" "text"
    _upload "otp_src_${stripped_name}.tar.gz" "application/gzip"
    _upload "otp_doc_html_${stripped_name}.tar.gz" "application/gzip"
    _upload "otp_doc_man_${stripped_name}.tar.gz" "application/gzip"
    _upload "otp_win32_${stripped_name}.exe" "application/x-msdownload"
    _upload "otp_win64_${stripped_name}.exe" "application/x-msdownload"
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
        if [[ $release < 24 ]]; then
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
        .github/scripts/release-docs.sh
        .github/scripts/create-artifacts.sh downloads "${name}"

        ## Delete any artifacts that we should not upload
        for artifact in dowloads/*; do
            if ! echo "${RI[@]}" | grep "${artifact}" 2> /dev/null > /dev/null; then
                rm -f "downloads/${artifact}"
            fi
        done
        _upload_artifacts "${name}"

        ## We only update one release per call to sync-github-releases
        break
    done
fi
