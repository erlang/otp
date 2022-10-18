#!/bin/bash

set -xe

CACHE_SOURCE_DIR="$1"
TARGET="$2"
ARCHIVE="$3"
EVENT="$4"
DELETED="$5"
CHANGES="$9"

if [ ! -f "${CACHE_SOURCE_DIR}/otp_src.tar.gz" ] || [ "${NO_CACHE}" = "true" ]; then
    cp "${ARCHIVE}" "${TARGET}"
    cp "${ARCHIVE}" "${CACHE_SOURCE_DIR}/otp_src.tar.gz"
    exit 0
fi

TMP_DIR=$(mktemp -d)
CACHE_DIR="${TMP_DIR}"
ARCHIVE_DIR="${TMP_DIR}/archive"

mkdir "${ARCHIVE_DIR}"

#################################
## START WORK ON THE CACHED FILES
#################################
echo "::group::{Restore cached files}"
tar -C "${CACHE_DIR}/" -xzf "${CACHE_SOURCE_DIR}/otp_src.tar.gz"

## If configure scripts have NOT changed, we can restore configure and other C/java programs
if [ -z "${CONFIGURE}" ] || [ "${CONFIGURE}" = "false" ]; then
    tar -C "${CACHE_DIR}/" -xzf "${CACHE_SOURCE_DIR}/otp_cache.tar.gz"
fi

## If bootstrap has been changed, we do not use the cached .beam files
EXCLUDE_BOOTSTRAP=""
if [ "${BOOTSTRAP}" = "true" ]; then
    find "${CACHE_DIR}/otp/lib" -name "*.beam" -exec rm -f {} \;
else
    EXCLUDE_BOOTSTRAP=(--exclude "bootstrap")
fi

## Make a copy of the cache for debugging
mkdir "${TMP_DIR}/cache"
cp -rp "${CACHE_DIR}/otp" "${TMP_DIR}/cache/"

CACHE_DIR="${CACHE_DIR}/otp"

echo "::group::{Delete files from PR}"
## Delete any files that this PR deletes
for delete in $DELETED; do
    if [ -d "${CACHE_DIR}/${delete}" ]; then
        rm -r "${CACHE_DIR}/${delete}"
    elif [ -f "${CACHE_DIR}/${delete}" ]; then
        rm "${CACHE_DIR}/${delete}"
    else
        echo "Could not find $delete to delete"
        exit 1
    fi
done

##################################
## START WORK ON THE UPDATED FILES
##################################

echo "::group::{Extract changed files}"
if [ -n "${ARCHIVE}" ]; then
    ## Extract with updated timestamp (the -m flag) so that any change will trigger a rebuild
    tar -C "${ARCHIVE_DIR}/" -xzmf "${ARCHIVE}"

    ## Directory permissions in the archive and cache are for some reason different...
    chmod -R g-w "${ARCHIVE_DIR}/"

    ## rlpgoD is the same as --archive, but without --times
    RSYNC_ARGS=(-rlpgoD --itemize-changes --verbose --checksum --update "${EXCLUDE_BOOTSTRAP[@]}" "${ARCHIVE_DIR}/otp/" "${CACHE_DIR}/")

    CHANGES="${TMP_DIR}/changes"
    PREV_CHANGES="${TMP_DIR}/prev-changes"

    touch "${PREV_CHANGES}"

    ## Below follows some rules about when we do not want to use the cache
    ## The rules are run multiple times so that if any rule triggeres a delte
    ## we will re-run the rules again with the new changes.
    for i in $(seq 1 10); do

        echo "::group::{Run ${i} at pruning cache}"

        ## First do a dry run to see if we need to delete anything from cache
        rsync --dry-run "${RSYNC_ARGS[@]}" | grep '^\(>\|c\)' > "${TMP_DIR}/changes"
        cat "${TMP_DIR}/changes"

        if cmp -s "${CHANGES}" "${PREV_CHANGES}"; then
            break;
        fi

        ### If any parse transform is changed we recompile everything as we have
        ### no idea what it may change. If the parse transform calls any other
        ### modules we really should delete the cache for those as well, but
        ### it is impossible for us to know which modules are used by a pt so
        ### this has to be somekind of best effort.
        echo "::group::{Run ${i}: parse transforms}"
        PARSE_TRANSFORMS=$(grep -r '^parse_transform(' "${CACHE_DIR}/" | grep "/lib/[^/]*/src/" | awk -F ':' '{print $1}' | uniq)
        for pt in $PARSE_TRANSFORMS; do
            if grep "$(basename "${pt}")" "${CHANGES}"; then
                echo "Deleting entire cache as a parse transform has changed" >&2
                rm -rf "${CACHE_DIR:?}/"
            fi
        done

        echo "::group::{Run ${i}: yecc}"
        ### if yecc has changed, need to recompile all .yrl files
        if grep "yecc.erl$" "${CHANGES}"; then
            echo "Deleting all .yrl files as yecc has changed" >&2
            find "${CACHE_DIR}/" -name "*.yrl" -exec rm -f {} \;
        fi

        echo "::group::{Run ${i}: asn1}"
        ### If asn1 has changed, need to re-compile all .asn1 files
        if grep lib/asn1 "${CHANGES}"; then
            echo "Deleting all .asn1 files as asn1 has changed" >&2
            find "${CACHE_DIR}/" -name "*.asn1" -exec rm -f {} \;
        fi

        echo "::group::{Run ${i}: docs}"
        ### If any of the doc generating tools change, we need to re-compile the docs
        if grep "lib/\(xmerl\|erl_docgen\|edoc\)" "${CHANGES}"; then
            echo "Deleting all docs as documentation tools have changed" >&2
            rm -rf "${CACHE_DIR}"/lib/*/doc/ "${CACHE_DIR}/erts/doc/" "${CACHE_DIR}/system/"
        fi

        ### Find all behaviours in OTP and check if any them as changed, we need to
        ### rebuild all files that use them.
        echo "::group::{Run ${i}: behaviours}"
        BEHAVIOURS=$(grep -r "^-callback" "${CACHE_DIR}/" | grep "/lib/[^/]*/src/" | awk -F ':' '{print $1}' | uniq | sed 's:.*/\([^/.]*\)[.]erl$:\1:')
        for behaviour in $BEHAVIOURS; do
            if grep "${behaviour}[.]erl\$" "${CHANGES}"; then
                echo "Deleting files using ${behaviour} has it has changed" >&2
                FILES=$(grep -r "^-behaviour(${behaviour})" "${CACHE_DIR}/" | grep "/lib/[^/]*/src/" | awk -F ':' '{print $1}')
                rm -f $FILES
            fi
        done

        if [ "$i" = "10" ]; then
            echo "Deleting entire cache as it did not stabalize in trime" >&2
            rm -rf "${CACHE_DIR:?}"
        else
            mv "${CHANGES}" "${PREV_CHANGES}"
        fi
    done

    echo "::group::{Sync changes over cached data}"

    ## Now we do the actual sync
    rsync "${RSYNC_ARGS[@]}"
fi

tar -czf "${TARGET}" -C "${TMP_DIR}" otp

rm -rf "${TMP_DIR}"

echo "::endgroup::"
