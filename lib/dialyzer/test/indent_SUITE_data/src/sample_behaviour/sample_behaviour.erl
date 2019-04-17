-module(sample_behaviour).

-type custom() :: 1..42.

-callback sample_callback_1() -> term().
-callback sample_callback_2() -> atom().
-callback sample_callback_3() -> {'ok', custom()} | 'fail'.

-callback sample_callback_4(term()) -> 'ok'.
-callback sample_callback_5(custom()) -> 'ok' | 'fail'.

-callback sample_callback_6(custom(), custom(), string()) ->
    {'ok', custom()} | 'fail'.
