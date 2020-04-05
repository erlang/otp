-module(sample_callback_correct).

-behaviour(sample_behaviour).

-export([
	 sample_callback_1/0,
	 sample_callback_2/0,
	 sample_callback_3/0,
	 sample_callback_4/1,
	 sample_callback_5/1,
	 sample_callback_6/3
	]).

sample_callback_1() -> 42.       % This is a valid return.
sample_callback_2() -> foo.      % This is a valid return.
sample_callback_3() -> {ok, 17}. % This is a valid return.
sample_callback_4(Input) ->
    put(mine, Input+1),          % This is valid handling of the input
    ok.                          % This is a valid return.
sample_callback_5(Input) ->
    case Input - 1 < 22 of       % This is valid handling of the input
	true  -> ok;             % This is a valid return.
	false -> fail            % This is a valid return.
    end.
sample_callback_6(OldNr, NewNr, Reason) ->
    Diff = NewNr - OldNr,                         % This is valid handling of the input
    Msg = string:join(["Reason: ", Reason], ","), % This is valid handling of the input
    case Diff > 0 of
	true -> put(mine, {NewNr, Msg}),
		{ok, NewNr};                      % This is a valid return.
	false -> fail                             % This is a valid return.
    end.
