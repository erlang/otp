-module(sample_callback_wrong).

%% This attribute uses the american spelling of 'behaviour'.
-behavior(sample_behaviour).

-export([
%	 sample_callback_1/0,
	 sample_callback_2/0,
	 sample_callback_3/0,
	 sample_callback_4/1,
	 sample_callback_5/1,
	 sample_callback_6/3
	]).

% sample_callback_1() -> 41.  % We can't really break this contract so: missing!
sample_callback_2() -> 42.    % This is not an atom().
sample_callback_3() -> fair.  % This is probably a typo.
sample_callback_4(_) ->       % We cannot break the input.
    fail.                     % We can definitely return a wrong value however. :)
sample_callback_5(Input) ->   % Input is treated as an atom, result is a list.
    atom_to_list(Input).      % Both violate the contract.
sample_callback_6(OldNr, NewNr, Reason) ->
    Diff = NewNr - OldNr, % This is valid handling of the input
    %% Reason should have been treated as a string.
    Msg = string:join(["Reason: ", atom_to_list(Reason)], ","),
    {okk, NewNr}. %% This, too, is a typo.
