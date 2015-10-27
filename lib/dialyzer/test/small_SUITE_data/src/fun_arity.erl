%%--------------------------------------------------------------------------
%% Module which contains calls to funs of different arity.
%%--------------------------------------------------------------------------
-module(fun_arity).

-export([f_0_ok/0, f_0_ko/0]).
-export([f_1_ok/0, f_1_ko/0]).

-export([fa_0_ok/0, fa_0_ko/0]).
-export([fa_1_ok/0, fa_1_ko/0]).

-export([mfa_0_ok/0, mfa_0_ko/0, mf/0]).
-export([mfa_1_ok/0, mfa_1_ko/0, mf/1]).

-export([mfa_ne_0_ok/0, mfa_ne_0_ko/0]).
-export([mfa_ne_1_ok/0, mfa_ne_1_ko/0]).

-export([mfa_nd_0_ok/0, mfa_nd_0_ko/0]).
-export([mfa_nd_1_ok/0, mfa_nd_1_ko/0]).

-export(['Mfa_0_ok'/1, 'Mfa_0_ko'/1]).
-export(['Mfa_1_ok'/1, 'Mfa_1_ko'/1]).

-export(['mFa_0_ok'/1, 'mFa_0_ko'/1]).
-export(['mFa_1_ok'/1, 'mFa_1_ko'/1]).

-export(['MFa_0_ok'/2, 'MFa_0_ko'/2]).
-export(['MFa_1_ok'/2, 'MFa_1_ko'/2]).

%%--------------------------------------------------------------------------

%% Funs like "fun(...) -> ... end".

f_0_ok() -> (fun_f_0())().
f_0_ko() -> (fun_f_0())(1).
fun_f_0() -> fun() -> ok end.

f_1_ok() -> (fun_f_1())(1).
f_1_ko() -> (fun_f_1())().
fun_f_1() -> fun(_) -> ok end .

%%--------------------------------------------------------------------------

%% Funs like "fun F/A" when F is literal atom and A is literal
%% non-negative integer.

fa_0_ok() -> (fun_fa_0())().
fa_0_ko() -> (fun_fa_0())(1).
fun_fa_0() -> fun f/0.
f() -> ok.

fa_1_ok() -> (fun_fa_1())(1).
fa_1_ko() -> (fun_fa_1())().
fun_fa_1() -> fun f/1.
f(_) -> ok.

%%--------------------------------------------------------------------------

%% Funs like "fun M:F/A" when M and F are literal atoms, A is literal
%% non-negative integer and function is (defined and) exported.

mfa_0_ok() -> (fun_mfa_0())().
mfa_0_ko() -> (fun_mfa_0())(1).
fun_mfa_0() -> fun ?MODULE:mf/0.
mf() -> ok.

mfa_1_ok() -> (fun_mfa_1())(1).
mfa_1_ko() -> (fun_mfa_1())().
fun_mfa_1() -> fun ?MODULE:mf/1.
mf(_) -> ok.

%% Funs like "fun M:F/A" when M and F are literal atoms, A is literal
%% non-negative integer and function is defined but not exported.

mfa_ne_0_ok() -> (fun_mfa_ne_0())().
mfa_ne_0_ko() -> (fun_mfa_ne_0())(1).
fun_mfa_ne_0() -> fun ?MODULE:mf_ne/0.
mf_ne() -> ok.

mfa_ne_1_ok() -> (fun_mfa_ne_1())(1).
mfa_ne_1_ko() -> (fun_mfa_ne_1())().
fun_mfa_ne_1() -> fun ?MODULE:mf_ne/1.
mf_ne(_) -> ok.

%% Funs like "fun M:F/A" when M and F are literal atoms, A is literal
%% non-negative integer and function is not defined.

mfa_nd_0_ok() -> (fun_mfa_nd_0())().
mfa_nd_0_ko() -> (fun_mfa_nd_0())(1).
fun_mfa_nd_0() -> fun ?MODULE:mf_nd/0.

mfa_nd_1_ok() -> (fun_mfa_nd_1())(1).
mfa_nd_1_ko() -> (fun_mfa_nd_1())().
fun_mfa_nd_1() -> fun ?MODULE:mf_nd/1.

%% Funs like "fun M:F/A" when M is variable, F is literal atoms and A
%% is literal non-negative integer.

'Mfa_0_ok'(M) -> ('fun_Mfa_0'(M))().
'Mfa_0_ko'(M) -> ('fun_Mfa_0'(M))(1).
'fun_Mfa_0'(M) -> fun M:f/0.

'Mfa_1_ok'(M) -> ('fun_Mfa_1'(M))(1).
'Mfa_1_ko'(M) -> ('fun_Mfa_1'(M))().
'fun_Mfa_1'(M) -> fun M:f/1.

%% Funs like "fun M:F/A" when M is literal atom, F is variable and A
%% is literal non-negative integer.

'mFa_0_ok'(F) -> ('fun_mFa_0'(F))().
'mFa_0_ko'(F) -> ('fun_mFa_0'(F))(1).
'fun_mFa_0'(F) -> fun ?MODULE:F/0.

'mFa_1_ok'(F) -> ('fun_mFa_1'(F))(1).
'mFa_1_ko'(F) -> ('fun_mFa_1'(F))().
'fun_mFa_1'(F) -> fun ?MODULE:F/1.

%% Funs like "fun M:F/A" when M and F are variables and A is literal
%% non-negative integer.

'MFa_0_ok'(M, F) -> ('fun_MFa_0'(M, F))().
'MFa_0_ko'(M, F) -> ('fun_MFa_0'(M, F))(1).
'fun_MFa_0'(M, F) -> fun M:F/0.

'MFa_1_ok'(M, F) -> ('fun_MFa_1'(M, F))(1).
'MFa_1_ko'(M, F) -> ('fun_MFa_1'(M, F))().
'fun_MFa_1'(M, F) -> fun M:F/1.
