%% -*- Mode: erlang; indent-tabs-mode: nil -*-
%% Copyright Ericsson AB 2017. All Rights Reserved.

%% Test that records are indented correctly

-record(record0,
        {
         r0a,
         r0b,
         r0c
        }).

-record(record1, {r1a,
                  r1b,
                  r1c
                 }).

-record(record2, {
                  r2a,
                  r2b
                 }).

-record(record3, {r3a = 8#42423 bor
                      8#4234,
                  r3b = 8#5432
                      bor 2#1010101,
                  r3c = 123 +
                      234,
                  r3d}).

-record(record5,
        { r5a = 1 :: integer()
        , r5b = foobar :: atom()
        }).

