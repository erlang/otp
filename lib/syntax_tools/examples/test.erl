%%
%% This is a test file
%%

-module(test).

-export([nrev/1]).

%% Just a naive reverse function in order
%% to get a code example with some comments.

nrev([X | Xs]) ->
    append(X, nrev(Xs));  % Quadratic behaviour
nrev([]) ->
    %% The trivial case:
    [].

  %% We need `append' as a subroutine:

append(Y, [X | Xs]) ->
    [X | append(Y, Xs)];    % Simple, innit?
append(Y, []) ->
    [Y].    % Done.

%% ---- end of file ----
