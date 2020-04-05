-module(sample_callback_correct_2).

-behaviour(sample_behaviour).

-export([
	 sample_callback_1/0,
	 sample_callback_2/0,
	 sample_callback_3/0,
	 sample_callback_4/1,
	 sample_callback_5/1,
	 sample_callback_6/3,
	 common_infrastructure/1
	]).

sample_callback_1() -> 42.       % This is a valid return.
sample_callback_2() -> halt().   % Crashes are also allowed.
sample_callback_3() -> {ok, 17}. % This is a valid return.
sample_callback_4(Input) ->
    case Input of
	1 -> common_infrastructure(Input); % This is 'correct' input for
	_ -> ok                            % common_infrastructure.
    end.
sample_callback_5(Input) ->
    case get(Input) of % This is valid handling of a more generic input
	true  -> ok;   % This is a valid return.
	false -> fail  % This is a valid return.
    end.
sample_callback_6(OldNr, NewNr, Reason) ->
    Diff = NewNr - OldNr,                         % This is valid handling of the input
    Msg = string:join(["Reason: ", Reason], ","), % This is valid handling of the input
    case Diff > 0 of
	true -> put(mine, {NewNr, Msg}),
		{ok, NewNr};                 % This is a valid return.
	false -> fail                        % This is a valid return.
    end.

common_infrastructure( 1) ->   'ok';
common_infrastructure(42) -> 'fail'.
