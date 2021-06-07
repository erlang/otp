%% @doc Fixtures for chunk signatures.
-module(eep48_sigs).

-export([f_sig_single_simple_clause/1,
	 f_sig_single_simple_clause_with_spec/1,
	 f_sig_multiple_simple_clauses/2,
	 f_sig_multiple_simple_clauses_with_spec/2,
	 f_sig_single_record_clause/1,
	 f_sig_single_record_clause_with_spec/1,
	 f_sig_multiple_record_clauses/1,
	 f_sig_multiple_record_clauses_with_spec/1]).

-record(r, {a, b}).
-type r() :: #r{}.

-record(s, {i, j}).
-type s() :: #s{}.

f_sig_single_simple_clause(Arg) -> ok.

-spec f_sig_single_simple_clause_with_spec(atom()) -> ok.
f_sig_single_simple_clause_with_spec(Arg) -> ok.

f_sig_multiple_simple_clauses(C1A1, C1A2) -> ok;
f_sig_multiple_simple_clauses(C2A1, C2A2) -> ok.

-spec f_sig_multiple_simple_clauses_with_spec(any(), any()) -> ok.
f_sig_multiple_simple_clauses_with_spec(C1A1, C1A2) -> ok;
f_sig_multiple_simple_clauses_with_spec(C2A1, C2A2) -> ok.

f_sig_single_record_clause(#r{a = _}) -> ok.

-spec f_sig_single_record_clause_with_spec(r()) -> ok.
f_sig_single_record_clause_with_spec(#r{a = _}) -> ok.

f_sig_multiple_record_clauses(#r{a = a}) -> ok;
f_sig_multiple_record_clauses(#s{i = i}) -> ok.

-spec f_sig_multiple_record_clauses_with_spec(r() | s()) -> ok.
f_sig_multiple_record_clauses_with_spec(#r{a = a}) -> ok;
f_sig_multiple_record_clauses_with_spec(#s{i = i}) -> ok.
