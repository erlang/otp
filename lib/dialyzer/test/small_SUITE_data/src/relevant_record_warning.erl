%% Formerly confusing_record_warning.erl.
%% The warning output is relevant as of Erlang/OTP 17.1.
%% The original comment kept below.

%%---------------------------------------------------------------------
%% A user complained that dialyzer produces a weird warning for the
%% following program.  I explained to him that there is an implicit
%% assumption that when a record is typed one cannot have types of
%% the same size which are tagged by the record name whose elements
%% have different types than the ones declared in the record.
%%
%% But the warning from dialyzer was weird nonetheless:
%%    The pattern {'r', [_]} can never match the type any()
%% We should clearly give some less confusing warning in this case.
%%---------------------------------------------------------------------
-module(relevant_record_warning).

-export([test/1]).

-record(r, {field :: binary}).

test({r, [_]}) ->
  #r{field = <<42>>}.
