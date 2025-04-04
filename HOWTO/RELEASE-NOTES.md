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

# Writing release notes

This HOWTO gives advice on how to write release notes.

The purpose of release notes is to inform users about changes,
improvements, bug fixes, and new features included in a new version of
Erlang/OTP. Therefore, they should generally only mention changes
observable by the user. Refactoring, fixing of spelling errors in
comments, or updates of test suites should in general not be
mentioned. One exception is when such change is the only change in the
application, in which case it is necessary to add a release note
anyway.

### Don't reuse the commit message as a release note

If one has followed the rules in [writing good commit
messages](https://github.com/erlang/otp/wiki/Writing-good-commit-messages),
a commit message is not suitable for direct inclusion in a release
note.

One reason is that the first line in a commit message is written in the
imperative mood, that is as a command or request. For example, the
first line of a commit message for the Erlang compiler could be:

    Combine creation of a record with subsequent record updates

Directly pasting this into a release note makes for strange reading,
as if the release note is asking the reader to perform the record
operations.

Another reason to avoid reusing a commit message is that it can
contain irrelevant information for a release note, for example why the
change was done or why it was done in a particular way, and it can
lack essential information.

### Examples of release notes

There is more than one way to rephrase the example from the previous
section to produce a suitable release note. For example:

    The compiler will now merge consecutive updates of the same record.

Another reasonable rewording is:

    The compiler has learned to merge consecutive updates of the same record.

Here are some other release note examples:

    Native coverage support has been implemented in the JIT.

    The documentation has been migrated to use Markdown and ExDoc.

    Safe destructive update of tuples has been implemented in
    the compiler and runtime system.

### Use examples and links to make release notes clearer

As of Erlang/OTP 27, release notes can be written in Markdown, making
it much easier to include examples and links. It is usually easier for
the writer of a release note to make their point clear using an
example instead of trying to express it using words, and it is often
easier for the reader to understand as well.

Example:

    The compiler will now merge consecutive updates of the same record.

    As an example, the body of the following function will be combined
    into a single tuple creation instruction:

    -record(r, {a,b,c,d}).

    ```erlang
    update(Value) ->
        R0 = #r{},
        R1 = R0#r{a=Value},
        R2 = R1#r{b=2},
        R2#r{c=3}.
    ```

Also include relevant links to the documentation, especially for a new
feature.

### Always include references to pull requests and issues

To further help the reader of the release note to understand a
particular change, always include references to the pull request and
to relevant issues.

The scripts that render release notes automatically turns references
to pull requests and Github issues to links. As an example, here is
the last line for the previous release note example:

    Own Id: OTP-18680 Aux Id: PR-7491, PR-8086, ERIERL-967

`PR-7491` and `PR-8086` are rendered as links.

Note that if a change requires a release note, it should generally
have a corresponding pull request. Exceptions are fixes for
vulnerabilities, which are often released without a pull request.

### Bug fixes

When the release note describes a fix to a bug, consider include the following:

* **Symptom** -- Describe the undesired effect of the bug. Like for example
  an incorrect function return value, an unexpectedly raised exception
  or a beam process crash.
* **Trigger** -- The conditions that are needed for the bug to manifest.
  Like specific function arguments or actions that need to happen in a specific
  order or at the same time.
* **Version** -- The oldest OTP version where the bug exists.
