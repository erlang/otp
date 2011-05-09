-module(guards).

-compile([export_all]).

-record(r, {f}).

%% This is the reduced original test (no warnings)

-define(g1(A), ((A#r.f =:= a) orelse (A#r.f =:= b))).

t1(A) when     ?g1(A) -> ok;
t1(A) when not ?g1(A) -> ko.

%% This should emit a warning

t1_s(A) when ?g1(A) -> ok.

t1_s_a() ->
    t1_s(#r{f=c}).

%% Same as t1 with 'or' instead of 'orelse' (no warnings)

-define(g2(A), ((A#r.f =:= a) or (A#r.f =:= b))).

t2(A) when     ?g2(A) -> ok;
t2(A) when not ?g2(A) -> ko.

%% This should emit a warning

t2_s(A) when ?g2(A) -> ok.

t2_s_a() ->
    t2_s(#r{f=c}).

%% This could probably give a warning

-define(g3(A), (A#r.f =:= a orelse is_atom(A))).

t3(A) when     ?g3(A) -> ok;
t3(A) when not ?g3(A) -> ko.

%% This could probably give a warning as well

-define(g4(A), ((A#r.f =:= a) or is_atom(A))).

t4(A) when     ?g4(A) -> ok;
t4(A) when not ?g4(A) -> ko.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some shots in the dark on guard abuse %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Should give a warning

t5(A) when is_atom(A) and is_integer(A) -> never.

%% Should give the same warning as t5

t6(A) when is_atom(A) andalso is_integer(A) -> never.

%% Should give a warning

t7(A) when is_atom(A) or is_integer(A) -> ok.

at7(42) -> t7(42);
at7('a') -> t7('a');
at7({42}) -> t7({42}).

%% Should give a warning

t8(A) when is_atom(A) orelse is_integer(A) -> ok.

at8(42) -> t8(42);
at8('a') -> t8('a');
at8({42}) -> t8({42}).

%% Should give a warning

t9(A) when is_atom(A) orelse is_integer(A) -> ok;
t9(A) when is_atom(A) -> redundant.

%% Should give a warning

t10(A) when is_atom(A) or is_integer(A) -> ok;
t10(A) when is_atom(A) -> redundant.

%% Should give a warning

t11(A, B) when is_atom(A) and is_atom(B) ->
    case {is_atom(A), is_atom(B)} of
	{true, true} -> ok;
	_ -> redundant
    end.

%% Should give a warning

t12(A, B) when is_atom(A) andalso is_atom(B) ->
    case {is_atom(A), is_atom(B)} of
	{true, true} -> ok;
	_ -> redundant
    end.

%% Should give two warnings

t13(A, B) when is_atom(A) or is_atom(B) ->
    case {is_atom(A), is_atom(B)} of
	{true , true } -> ok;
	{true , false} -> ok;
	{false, true } -> ok;
	{false, false} -> never;
	{_    , _    } -> even_more_never
    end.

%% Should give two warnings

t14(A, B) when is_atom(A) orelse is_atom(B) ->
    case {is_atom(A), is_atom(B)} of
	{true , true } -> ok;
	{true , false} -> ok;
	{false, true } -> ok;
	{false, false} -> never;
	{_    , _    } -> even_more_never
    end.

%% Should give two warnings

t15(A) when ((A =:= a) or (A =:= b)) and ((A =:= b) or (A =:= c)) -> ok.

t15_a() -> t15(a), t15(b), t15(c).

%% Should give two warnings

t16(A) when ((A =:= a) orelse (A =:= b)) andalso
	    ((A =:= b) orelse (A =:= c)) -> ok.

t16_a() -> t16(a), t16(b), t16(c).
