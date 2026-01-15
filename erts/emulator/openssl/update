#!/bin/bash

# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2025. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%

# Files and directories with files taken from OpenSSL
openssl_files_root="LICENSE.txt crypto include"

ei_openssl="../../../lib/erl_interface/src/openssl"
repo="https://github.com/openssl/openssl"
ossl_clone=openssl_clone
no_commit=false

temp_note=`cat <<HERE
  Note that all temporary files always will be left as is, even after
  successful completion (including the temporary OpenSSL repository
  $ossl_clone). This in order for you to be able to inspect this data.
  You always want to verify that the result is sane. Temporary files
  can be removed by calling this script with the 'cleanup' argument.
HERE`
usage_text=`cat <<HERE
  Usage:
    $ ./update [help|cleanup|no-commit|finalize]

  - help      - Print this information.
  - cleanup   - Remove all temporary files produced.
  - no-commit - Stop after patching even if patching was successful. After
                adding all files that should be included, restart by calling
                the script with the 'finalize' argument.
  - finalize  - Mirror all added files to the lib/erl_interface/src/openssl/
                directory, update and add the vendor.info files, and then
                commit the results.

  This script will diff the changes made in OpenSSL between the old version
  and the last version for the files take from OpenSSL and then try to patch
  these files. If the patching fails, the script will stop and ask you to
  manually solve the conflicts, in the subdirectories of this directory, and
  add the changes made to these files. When this has been done, run the
  script and pass it the 'finalize' argument.

$temp_note
HERE`
patch_failed_text=`cat <<HERE

  You need to manually solve this. When done add all files that should
  be changed (except the vendor.info file). Then run "./update finalize"
  which will mirror the changes to lib/erl_interface/src/openssl/ and
  commit the changes.

$temp_note
HERE`
success_note=`cat <<HERE
  Note that successful patching does not mean that it is correct or works.
  Inspect the changes that were made and test it properly, before including
  the changes into a patch or release. Make sure any potential changes to
  the license are acceptable.

$temp_note
HERE`

cleanup () {
    /bin/rm -rf ./openssl_clone
    /bin/rm -f ./openssl.diff
    /bin/rm -f ./update.diff
    /bin/rm -f ./update_from_version
    /bin/rm -f ./update_to_version
    /bin/rm -f ./update_to_sha
    /bin/rm -f ./include/*/*.{rej,orig}
    /bin/rm -f ./crypto/*/*.{rej,orig}
    exit 0;
}

print_error () {
    type printf >/dev/null 2>&1
    if [ $? -eq 0 ]; then
	printf "\033[31mERROR: %s\033[0m\n" "$@" 1>&2
    else
	echo "ERROR: $@" 1>&2
    fi
}

fail () {
    print_error "$@"
    echo "$temp_note" 1>&2
    exit 1
}

usage () {
    if [ $# -gt 0 ]; then
        print_error "$@"
        echo "$usage_text" 1>&2
        exit 1
    else
        echo "$usage_text"
        exit 0
    fi
}

is_openssl_file () {
    for file in $files; do
        if [ "$file" = "$1" ]; then
            return 0
        fi
    done
    return 1
}

update_vendor_info () {
    vendor_info_file="$1"
    cmnt=`cat "$vendor_info_file" | grep "^//"` || {
        fail "Failed to read comments in $vendor_info_file"
    }
    new_vinfo=`cat "$vendor_info_file" | grep -v "^//" | jq "map(.versionInfo = \"${new_vsn}\" | .sha = \"${new_sha}\")"` || {
        fail "Failed to update vendor information"
    }

cat <<EOF > "$vendor_info_file"
${cmnt}
${new_vinfo}
EOF

[ $? -eq 0 ] || fail "Failed to write updated vendor info to $vendor_info_file"

git add "$vendor_info_file" || fail "Failed to add $vendor_info_file"

}

finalize () {

    [ -e ./update_from_version ] || fail "No from version found"
    old_vsn=`cat ./update_from_version`
    case $old_vsn in
        *.*.*) ;;
        *) fail "Unexpected from version: $old_vsn";;
    esac
    [ -e ./update_to_version ] || fail "No to version found"
    new_vsn=`cat ./update_to_version`
    case $new_vsn in
        *.*.*) ;;
        *) fail "Unexpected to version: $new_vsn";;
    esac
    [ -e ./update_to_version ] || fail "No to sha found"
    new_sha=`cat ./update_to_sha`
    if [ "$new_sha" = "" ]; then
        fail "Empty to sha"
    fi
    # Copy added files to erl_interface as well"
    added_files=`git diff --name-only --cached` || fail "Failed to get staged files"
    for added_file in $added_files; do
        case $added_file in
            erts/emulator/openssl/*)
                f=`echo $added_file | sed 's|^erts/emulator/openssl/||'`
                if is_openssl_file "$f"; then
                    /bin/cp "$f" "$ei_openssl/$f" || fail "Failed to copy '$f' to '$ei_openssl/$f'"
                    git add "$ei_openssl/$f" || fail "Failed to add '$ei_openssl/$f'"
                fi;;
            *)
            ;;
        esac
    done

    update_vendor_info "./vendor.info"
    update_vendor_info "$ei_openssl/vendor.info"

    git commit -m"Update MD5 implementation from OpenSSL $old_vsn to $new_vsn" || {
        fail "Failed to commit changes"
    }
    success "Patched MD5 implementations"
}

success () {
    type printf >/dev/null 2>&1
    if [ $? -eq 0 ]; then
	printf "\033[32mSUCCESS: %s\033[0m\n" "$@" 1>&2
    else
	echo "SUCCESS: $@" 1>&2
    fi
    echo ""
    echo "$success_note"
    exit 0
}

repo_dir=`git rev-parse --show-prefix 2>&1` || {
    fail "Not in a git repository"
}

[ "$repo_dir" = "erts/emulator/openssl/" ] || {
    fail "Current working directory in the repository should be erts/emulator/openssl"
}

files=`git ls-files $openssl_files_root` || fail "git ls-files failed"

while [ $# -gt 0 ]; do
    case "$1" in
        help) usage;;
        cleanup) cleanup;;
        no-commit) no_commit=true;;
        finalize) finalize;;
        *) usage "Unexpected argument $1";;
    esac
    shift
done

old_vsn=`cat ./vendor.info | grep -v "^//" | jq '.[] | .versionInfo' | sed  s'/"//g' | sed s'/^\([0-9][0-9]*\.[0-9][0-9]*\)$/\1.0/'` || {
    fail "Failed to read old version number from ./vendor.info"
}

case $old_vsn in
    *.*.*) ;;
    *) fail "Unexpected version number in ./vendor.info: $old_vsn";;
esac

echo $old_vsn > ./update_from_version

old_tag="openssl-$old_vsn"

[ -d $ossl_clone ] || git clone $repo $ossl_clone || fail "Failed to clone $repo"

cd $ossl_clone || fail "Failed to cd into cloned repo $ossl_clone"

new_tag=`git tag -l openssl-*| grep -E ^openssl-[\.0-9]+$|sort -V|tail -n 1` || {
    fail "Failed to get last tag from $ossl_clone"
}

[ "$old_tag" != "$new_tag" ] || {
    success "Nothing to update; already at last version"
}

new_vsn=`echo $new_tag | sed 's/^openssl-\([\.0-9]*\)$/\1/'`

case $new_vsn in
    *.*.*) ;;
    *) fail "Unexpected last version in $ossl_clone: $new_vsn";;
esac

echo $new_vsn > ../update_to_version

new_sha=`git rev-parse $new_tag^{commit}` || fail "Failed to get commit sha of $new_tag"

echo $new_sha > ../update_to_sha

openssl_files=
for file in $files; do
    case $file in
        include/openssl_local/*)
            openssl_file=`echo $file | sed 's|include/openssl_local/|include/openssl/|'` || {
                fail "Failed to sed openssl directory name"
            }
            openssl_files="$openssl_files $openssl_file";;
        *)
            openssl_files="$openssl_files $file";;
    esac
done

git diff -u $old_tag $new_tag -- $openssl_files > ../openssl.diff || {
    fail "Failed to diff files '$openssl_files' between $old_tag $new_tag tags"
}

cd .. || fail "Failed to change directory into original directory"
# Back in erts/emulator/openssl"

cat ./openssl.diff | sed 's|include/openssl/|include/openssl_local/|g' > ./update.diff || {
    fail "Failed to modify update.diff"
}

patch -p1 <./update.diff || {
    print_error "Failed to patch local files"
    echo "$patch_failed_text" 1>&2
    exit 1
}

[ $no_commit = false ] || {
    echo "Patching succeeded, but the 'no-commit' argument was passed"
    echo "$patch_failed_text"
    exit 0
}

finalize
