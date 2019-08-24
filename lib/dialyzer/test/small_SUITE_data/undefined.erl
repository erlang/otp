-module(undefined).

-export([t/0]).

%% As of OTP 19.0 'undefined' is no longer added to fields with a type
%% declaration but without an initializer. The pretty printing of
%% records (erl_types:t_to_string()) is updated to reflect this: if a
%% field is of type 'undefined', it is output if 'undefined' is not in
%% the declared type of the field. (It used to be the case that the
%% singleton type 'undefined' was never output.)
%%
%% One consequence is shown by the example below: the warning about
%% the record construction violating the the declared type shows
%% #r{..., d::'undefined', ...} which is meant to be of help to the
%% user, who could otherwise get confused the first time (s)he gets
%% confronted by the warning.

-record(r,
        {
          a = {fi},
          b = {a,b} :: list(),                  % violation
          c = {a,b} :: list(),
          d :: list(),                          % violation
          e = [] :: list(),
          f = undefined :: list()               % violation
        }).

t() ->
    #r{c = []}.
