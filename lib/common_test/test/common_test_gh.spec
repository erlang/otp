% SPDX-FileCopyrightText: 2025 Erlang/OTP and contributors
%
% SPDX-License-Identifier: Apache-2.0
{suites,"../common_test_test",all}.
{skip_suites,"../common_test_test",[ct_release_test_SUITE],"Versions not always correct"}.
{event_handler, {cte_track, []}}.
{enable_builtin_hooks, false}.
