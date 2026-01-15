-module(converted_metadata).

-moduledoc(#{ authors => ["me","myself","I"], format => "custom", since => "1.0", deprecated => "yes" }).

-export([test/0]).

-doc #{ since => "1.0", deprecated => "yes", equiv => "other", group => "collection" }.
test() -> ok.

% SPDX-License-Identifier: Apache-2.0
% SPDX-FileCopyrightText: 2024 Erlang/OTP and contributors