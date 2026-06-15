#!/usr/bin/env sh

## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0
##
## Copyright Ericsson AB 2026-2025. All Rights Reserved.
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

# Note: This script is to be called internally as follows:
# openvex-sync.yaml -->
#    .github/scripts/otp-compliance.es vex verify -p -->
#        create-openvex-pr.sh
#

set -e   # exit immediately if any command fails

REPO=$1
BRANCH_NAME=$2
ORPHAN_BRANCH="openvex"
OTP_REMOTE="${3:-origin}"

# Fetch PR data using gh CLI
PR_STATUS=$(gh pr view "$BRANCH_NAME" --repo "$REPO" --json state -q ".state")
FOUND_PR=$?

if [ "$FOUND_PR" -ne 0 ]; then
  echo "No PR with name #$BRANCH_NAME in $REPO exists."
  echo "A new PR will be created"
fi

if [ "$PR_STATUS" = "CLOSED" ] || [ "$PR_STATUS" = "MERGED" ] || [ "$FOUND_PR" -ne 0 ]; then
  echo "Pull request #$BRANCH_NAME is CLOSED or MERGED."
  echo "✅ A new pull request with name #$BRANCH_NAME will be created."

  # Fetch the orphan branch
  git fetch "$OTP_REMOTE" "$ORPHAN_BRANCH"

  # Create a worktree for the orphan branch in a temp directory
  # This avoids switching branches in the current working tree
  WORKTREE=$(mktemp -d /tmp/openvex-worktree.XXXXXX)
  git worktree add "$WORKTREE" "$ORPHAN_BRANCH"

  # Create a new work branch inside the worktree
  git -C "$WORKTREE" checkout -b "$BRANCH_NAME"

  # Copy the generated files into the worktree
  cp otp-*.openvex.json         "$WORKTREE/" 2>/dev/null || true
  cp otp-*.openvex.json.license "$WORKTREE/" 2>/dev/null || true
  cp openvex.table              "$WORKTREE/" 2>/dev/null || true

  # Commit inside the worktree
  git -C "$WORKTREE" add otp-*.openvex.json
  git -C "$WORKTREE" add otp-*.openvex.json.license
  git -C "$WORKTREE" add openvex.table
  git -C "$WORKTREE" commit -m "Automatic update of OpenVEX Statements for erlang/otp"

  # Push from the worktree since the branch was created there
  git -C "$WORKTREE" push --force origin "$BRANCH_NAME"

  gh pr create --repo "$REPO" -B "$ORPHAN_BRANCH" \
               --head "$BRANCH_NAME" \
               --title "Automatic update of OpenVEX Statements for erlang/otp" \
               --body "Automatic Action. There is a vulnerability from GH Advisories without a matching OpenVEX statement"
else
  echo "❌ Pull request #$BRANCH_NAME is OPEN. Create a PR once the PR is closed or merged."
  exit 0
fi
