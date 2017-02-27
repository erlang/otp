%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

-module(xref_SUITE).

%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), "./log_dir/").
-define(t,test_server).
-define(datadir, "xref_SUITE_data").
-define(privdir, "xref_SUITE_priv").
-define(copydir, "xref_SUITE_priv/datacopy").
-else.
-include_lib("common_test/include/ct.hrl").
-define(format(S, A), ok).
-define(datadir, proplists:get_value(data_dir, Conf)).
-define(privdir, proplists:get_value(priv_dir, Conf)).
-define(copydir, proplists:get_value(copy_dir, Conf)).
-endif.

-export([all/0, suite/0, groups/0,
         init_per_suite/1, end_per_suite/1]).

-export([addrem/1, convert/1, intergraph/1, lines/1, loops/1,
         no_data/1, modules/1]).

-export([add/1, default/1, info/1, lib/1, read/1, read2/1, remove/1,
         replace/1, update/1, deprecated/1, trycatch/1,
         fun_mfa/1, fun_mfa_r14/1,
         fun_mfa_vars/1, qlc/1]).

-export([analyze/1, basic/1, md/1, q/1, variables/1, unused_locals/1]).

-export([format_error/1, otp_7423/1, otp_7831/1, otp_10192/1, otp_13708/1]).

-import(lists, [append/2, flatten/1, keysearch/3, member/2, sort/1, usort/1]).

-import(sofs, [converse/1, from_term/1, intersection/2, is_sofs_set/1,
               range/1, relation_to_family/1, set/1, to_external/1,
               union/2]).

%% Checks some info counters of a server and some relations that should hold.
-export([check_count/1, check_state/1]).

-include_lib("kernel/include/file.hrl").
-include_lib("tools/src/xref.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [{group, xref}, {group, files}, {group, analyses},
     {group, misc}].

groups() -> 
    [{xref, [],
      [addrem, convert, intergraph, lines, loops, no_data,
       modules]},
     {files, [],
      [add, default, info, lib, read, read2, remove, replace,
       update, deprecated, trycatch, fun_mfa,
       fun_mfa_r14, fun_mfa_vars, qlc]},
     {analyses, [],
      [analyze, basic, md, q, variables, unused_locals]},
     {misc, [], [format_error, otp_7423, otp_7831, otp_10192, otp_13708]}].


init_per_suite(Conf) when is_list(Conf) ->
    DataDir = ?datadir,
    PrivDir = ?privdir,
    CopyDir = fname(PrivDir, "datacopy"),
    ok = file:make_dir(CopyDir),
    TarFile = fname(PrivDir, "datacopy.tgz"),
    ok = file:set_cwd(DataDir),
    ok = erl_tar:create(TarFile, ["."], [compressed]),
    ok = erl_tar:extract(TarFile, [compressed, {cwd,CopyDir}]),
    ok = file:delete(TarFile),
    [{copy_dir, CopyDir}|Conf].

end_per_suite(Conf) when is_list(Conf) ->
    ok.

%% Seems a bit short...
%% Simple test of removing modules
addrem(Conf) when is_list(Conf) ->
    S0 = new(),

    F1 = {m1,f1,1},
    F2 = {m2,f1,2},

    E1 = {F1,F2},
    E2 = {F2,F1},

    D1 = {F1,12},
    DefAt_m1 = [D1],
    X_m1 = [F1],
    % L_m1 = [],
    XC_m1 = [E1],
    LC_m1 = [],
    LCallAt_m1 = [],
    XCallAt_m1 = [{E1,13}],
    Info1 = #xref_mod{name = m1, app_name = [a1]},
    S1 = add_module(S0, Info1, DefAt_m1, X_m1, LCallAt_m1, XCallAt_m1,
                    XC_m1, LC_m1),

    D2 = {F2,7},
    DefAt_m2 = [D2],
    X_m2 = [F2],
    % L_m2 = [],
    XC_m2 = [E2],
    LC_m2 = [],
    LCallAt_m2 = [],
    XCallAt_m2 = [{E2,96}],
    Info2 = #xref_mod{name = m2, app_name = [a2]},
    S2 = add_module(S1, Info2, DefAt_m2, X_m2, LCallAt_m2, XCallAt_m2,
                    XC_m2, LC_m2),

    S5 = set_up(S2),

    {ok, XMod1, S6} = remove_module(S5, m1),
    [a1] = XMod1#xref_mod.app_name,
    {ok, XMod2, S6a} = remove_module(S6, m2),
    [a2] = XMod2#xref_mod.app_name,
    S7 = set_up(S6a),

    AppInfo1 = #xref_app{name = a1, rel_name = [r1]},
    S9 = add_application(S7, AppInfo1),
    S10 = set_up(S9),
    AppInfo2 = #xref_app{name = a2, rel_name = [r1]},
    _S11 = add_application(S10, AppInfo2),
    ok.

%% Coercion of data
convert(Conf) when is_list(Conf) ->
    S0 = new(),

    F1 = {m1,f1,1},
    F6 = {m1,f2,6}, % X
    F2 = {m2,f1,2},
    F3 = {m2,f2,3}, % X
    F7 = {m2,f3,7}, % X
    F4 = {m3,f1,4}, % X
    F5 = {m3,f2,5},

    UF1 = {m1,f12,17},
    UF2 = {m17,f17,177},

    E1 = {F1,F3}, % X
    E2 = {F6,F7}, % X
    E3 = {F2,F6}, % X
    E4 = {F1,F4}, % X
    E5 = {F4,F5},
    E6 = {F7,F4}, % X

    UE1 = {F2,UF2}, % X
    UE2 = {F5,UF1}, % X

    D1 = {F1,12},
    D6 = {F6,3},
    DefAt_m1 = [D1,D6],
    X_m1 = [F6],
    % L_m1 = [F1],
    XC_m1 = [E1,E2,E4],
    LC_m1 = [],
    LCallAt_m1 = [],
    XCallAt_m1 = [{E1,13},{E2,17},{E4,7}],
    Info1 = #xref_mod{name = m1, app_name = [a1]},
    S1 = add_module(S0, Info1, DefAt_m1, X_m1, LCallAt_m1, XCallAt_m1,
                    XC_m1, LC_m1),

    D2 = {F2,7},
    D3 = {F3,9},
    D7 = {F7,19},
    DefAt_m2 = [D2,D3,D7],
    X_m2 = [F3,F7],
    % L_m2 = [F2],
    XC_m2 = [E3,E6,UE1],
    LC_m2 = [],
    LCallAt_m2 = [],
    XCallAt_m2 = [{E3,96},{E6,12},{UE1,77}],
    Info2 = #xref_mod{name = m2, app_name = [a2]},
    S2 = add_module(S1, Info2, DefAt_m2, X_m2, LCallAt_m2, XCallAt_m2,
                    XC_m2, LC_m2),

    D4 = {F4,6},
    D5 = {F5,97},
    DefAt_m3 = [D4,D5],
    X_m3 = [F4],
    % L_m3 = [F5],
    XC_m3 = [UE2],
    LC_m3 = [E5],
    LCallAt_m3 = [{E5,19}],
    XCallAt_m3 = [{UE2,22}],
    Info3 = #xref_mod{name = m3, app_name = [a3]},
    S3 = add_module(S2, Info3, DefAt_m3, X_m3, LCallAt_m3, XCallAt_m3,
                    XC_m3, LC_m3),

    Info4 = #xref_mod{name = m4, app_name = [a2]},
    S4 = add_module(S3, Info4, [], [], [], [], [], []),

    AppInfo1 = #xref_app{name = a1, rel_name = [r1]},
    S9 = add_application(S4, AppInfo1),
    AppInfo2 = #xref_app{name = a2, rel_name = [r1]},
    S10 = add_application(S9, AppInfo2),
    AppInfo3 = #xref_app{name = a3, rel_name = [r2]},
    S11 = add_application(S10, AppInfo3),

    RelInfo1 = #xref_rel{name = r1},
    S12 = add_release(S11, RelInfo1),
    RelInfo2 = #xref_rel{name = r2},
    S13 = add_release(S12, RelInfo2),

    S = set_up(S13),

    {ok, _} = eval("(Lin)(m1->m1:Mod) * m1->m1", type_error, S),
    {ok, _} = eval("(XXL)(Lin)(m1->m1:Mod) * m1->m1", type_error, S),

    AllDefAt = eval("(Lin) M", S),
    AllV = eval("(Fun) M", S),
    AllCallAt = eval("(XXL)(Lin) E", S),
    AllE = eval("E", S),

    AM = eval("AM", S),
    A = eval("A", S),
    R = eval("R", S),


    % vertices
    % general 1 step
    {ok, _} = eval("(Fun) (Lin) M", AllV, S),
    {ok, _} = eval("(Fun) (Lin) (Lin) M", AllV, S),
    {ok, _} = eval(f("(Fun) (Lin) ~p", [[F1, F3]]), [F1,F3], S),
    {ok, _} = eval(f("(Mod) ~p", [AllV]), [m1,m17,m2,m3], S),
    {ok, _} = eval(f("(Mod) ~p", [[F1,F3,F6]]), [m1,m2], S),
    {ok, _} = eval("(App) M", A, S),
    {ok, _} = eval(f("(App) ~p", [[m1,m2,m4]]), [a1,a2], S),
    {ok, _} = eval(f("(Rel) ~p", [A]), R, S),
    {ok, _} = eval(f("(Rel) ~p", [[a1,a2,a2]]), [r1], S),
    % general 2 steps
    {ok, _} = eval("(Mod) (Lin) M", [m1,m17,m2,m3], S),
    {ok, _} = eval(f("(App) ~p", [AllV]), [a1,a2,a3], S),
    {ok, _} = eval("(Rel) M", R, S),
    % general 4 steps
    {ok, _} = eval("(Rel) (Lin) M", [r1,r2], S),

    % special 1 step
    {ok, _} = eval(f("(Lin) ~p", [AllV]), AllDefAt, S),
    {ok, _} = eval(f("(Lin) ~p", [[F1,F3]]), [{F1,12},{F3,9}], S),
    {ok, _} = eval("(Fun) M", AllV, S),
    {ok, _} = eval(f("(Fun) ~p", [[m1,m2]]), [F1,F2,F3,F6,F7,UF1], S),
    {ok, _} = eval(f("(Mod) ~p", [A]), AM, S),
    {ok, _} = eval(f("(Mod) ~p", [[a1,a2]]), [m1,m2,m4], S),
    {ok, _} = eval(f("(App) ~p", [R]), A, S),
    {ok, _} = eval(f("(App) ~p", [[r1]]), [a1,a2], S),
    % special 2 steps
    {ok, _} = eval("(Lin) M", AllDefAt, S),
    AnalyzedV = eval("(Fun) AM", S),
    {ok, _} = eval(f("(Fun) ~p", [A]), AnalyzedV, S),
    {ok, _} = eval(f("(Mod) ~p", [R]), AM, S),
    % special 4 steps
    AnalyzedAllDefAt = eval("(Lin) AM", S),
    {ok, _} = eval("(Lin) R", AnalyzedAllDefAt, S),

    % edges
    Ms = [{m1,m2},{m1,m3},{m2,m1},{m2,m3},{m3,m3}],
    UMs = [{m2,m17},{m3,m1}],
    AllMs = append(Ms, UMs),
    As = [{a1,a2},{a1,a3},{a2,a1},{a2,a3},{a3,a3}],
    Rs = [{r1,r1},{r1,r2},{r2,r2}],

    % general 1 step
    {ok, _} = eval("(Fun) (Lin) E", AllE, S),
    {ok, _}  = eval(f("(Fun)(Lin) ~p", [[E1, E6]]), [E1, E6], S),
    {ok, _} = eval("(Mod) E", AllMs, S),
    {ok, _} = eval(f("(Mod) ~p", [[E1, E6]]), [{m1,m2},{m2,m3}], S),
    {ok, _} = eval(f("(App) ~p", [As]), As, S),
    {ok, _} = eval("(App) [m1->m2,m2->m3]", [{a1,a2},{a2,a3}], S),
    {ok, _} = eval(f("(Rel) ~p", [As]), Rs, S),
    {ok, _} = eval("(Rel) a1->a2", [{r1,r1}], S),

    % special 1 step
    {ok, _} = eval("(XXL) (Lin) (Fun) E", AllCallAt, S),
    {ok, _} = eval("(XXL) (XXL) (Lin) (Fun) E", AllCallAt, S),

    {ok, _} = eval(f("(XXL) (Lin) ~p", [[E1, E6]]),
                   [{{D1,D3},[13]}, {{D7,D4},[12]}], S),
    {ok, _} = eval(f("(Fun) ~p", [AllMs]), AllE, S),
    {ok, _} = eval("(Fun) [m1->m2,m2->m3]", [E1,E2,E6], S),
    {ok, _} = eval(f("(Mod) ~p", [As]), Ms, S),
    {ok, _} = eval("(Mod) [a1->a2,a2->a3]", [{m1,m2},{m2,m3}], S),
    {ok, _} = eval(f("(App) ~p", [Rs]), As, S),
    {ok, _} = eval("(App) r1->r1", [{a1,a2},{a2,a1}], S),
    ok.

%% Inter Call Graph
intergraph(Conf) when is_list(Conf) ->
    S0 = new(),

    F1 = {m1,f1,1}, % X
    F2 = {m1,f2,2}, % X
    F3 = {m1,f3,3},
    F4 = {m1,f4,4},
    F5 = {m1,f5,5},

    F6 = {m2,f1,6}, % X
    F7 = {m2,f1,7},
    F8 = {m2,f1,8},
    F9 = {m2,f1,9},
    F10 = {m2,f1,10},
    F11 = {m2,f1,11},

    % Note: E1 =:= E4!
    E1 = {F2,F1},
    E2 = {F2,F3},
    E3 = {F3,F1},
    E4 = {F2,F1}, % X
    E5 = {F4,F2},
    E6 = {F5,F4},
    E7 = {F4,F5},

    E8 = {F6,F7},
    E9 = {F7,F8},
    E10 = {F8,F1}, % X
    E11 = {F6,F9},
    E12 = {F6,F10},
    E13 = {F9,F11},
    E14 = {F10,F11},
    E15 = {F11,F1}, % X

    D1 = {F1,1},
    D2 = {F2,2},
    D3 = {F3,3},
    D4 = {F4,4},
    D5 = {F5,5},
    DefAt_m1 = [D1,D2,D3,D4,D5],
    X_m1 = [F1,F2],
    % L_m1 = [F3,F4,F5],
    XC_m1 = [E4],
    LC_m1 = [E1,E2,E3,E5,E6,E7],
    % Note: E1 and E4 together!
    LCallAt_m1 = [{E1,1},{E2,2},{E3,3},{E5,5},{E6,6},{E7,7}],
    XCallAt_m1 = [{E1,4}],
    Info1 = #xref_mod{name = m1, app_name = [a1]},
    S1 = add_module(S0, Info1, DefAt_m1, X_m1, LCallAt_m1, XCallAt_m1,
                    XC_m1, LC_m1),

    D6 = {F6,6},
    D7 = {F7,7},
    D8 = {F8,8},
    D9 = {F9,9},
    D10 = {F10,10},
    D11 = {F11,11},
    DefAt_m2 = [D6,D7,D8,D9,D10,D11],
    X_m2 = [F6],
    % L_m2 = [F7,F8,F9,F10,F11],
    XC_m2 = [E10,E15],
    LC_m2 = [E8,E9,E11,E12,E13,E14],
    LCallAt_m2 = [{E8,8},{E9,9},{E11,11},{E12,12},{E13,13},{E14,14}],
    XCallAt_m2 = [{E10,10},{E15,15}],
    Info2 = #xref_mod{name = m2, app_name = [a2]},
    S2 = add_module(S1, Info2, DefAt_m2, X_m2, LCallAt_m2, XCallAt_m2,
                    XC_m2, LC_m2),

    AppInfo1 = #xref_app{name = a1, rel_name = [r1]},
    S5 = add_application(S2, AppInfo1),
    AppInfo2 = #xref_app{name = a2, rel_name = [r1]},
    S6 = add_application(S5, AppInfo2),

    RelInfo = #xref_rel{name = r1},
    S7 = add_release(S6, RelInfo),

    S = set_up(S7),

    {ok, _} = eval("EE | m1", [E1,E5,E6,E7], S),
    {ok, _} = eval("EE | m2", [{F6,F1}], S),
    {ok, _} = eval("EE | m2 + EE | m2", [{F6,F1}], S),

    {ok, _} = eval("(Fun)(Lin)(E | m1)",
                   to_external(union(set(XC_m1), set(LC_m1))), S),
    {ok, _} = eval("(XXL)(ELin) (EE | m1)",
                   [{{D2,D1},[1,2,4]},{{D4,D2},[5]},{{D5,D4},[6]},{{D4,D5},[7]}],
                   S),
    {ok, _} = eval("(XXL)(ELin)(EE | m2)", [{{D6,D1},[8,11,12]}], S),
    {ok, _} = eval("(XXL)(ELin)(ELin)(EE | m2)",
                   [{{D6,D1},[8,11,12]}], S),

    %% Combining graphs (equal or different):
    {ok, _} = eval("(XXL)(ELin)(EE | m2 + EE | m2)",
                   [{{D6,D1},[8,11,12]}], S),
    {ok, _} = eval("(XXL)(ELin)(EE | m2 * EE | m2)",
                   [{{D6,D1},[8,11,12]}], S),
    {ok, _} = eval("(XXL)(ELin)(EE | m2 - EE | m1)",
                   [{{D6,D1},[8,11,12]}], S),
    {ok, _} = eval("(XXL)(ELin)(EE | m2 - E | m2)",
                   [{{D6,D1},[8,11,12]}], S),
    {ok, _} = eval("(XXL)(ELin)(Fun)(ELin)(EE | m2)",
                   [{{D6,D1},[8,11,12]}], S),
    {ok, _} = eval("EE | m1 + E | m1", LC_m1, S),
    {ok, _} = eval(f("EE | ~p + E | ~p", [F2, F2]), [E1,E2], S),
    %% [1,4] from 'calls' is a subset of [1,2,4] from Inter Call Graph:
    {ok, _} = eval(f("(XXL)(Lin) (E | ~p)", [F2]),
                   [{{D2,D1},[1,4]},{{D2,D3},[2]}], S),

    {ok, _} = eval(f("(XXL)(ELin) (EE | ~p)", [F2]),
                   [{{D2,D1},[1,2,4]}], S),
    {ok, _} = eval(f("(XXL)((ELin)(EE | ~p) + (Lin)(E | ~p))", [F2, F2]),
                   [{{D2,D1},[1,2,4]},{{D2,D3},[2]}], S),
    {ok, _} =
    eval(f("(XXL)((ELin) ~p + (Lin) ~p)", [{F2, F1}, {F2, F1}]),
         [{{D2,D1},[1,2,4]}], S),
    {ok, _} = eval(f("(Fun)(Lin) ~p", [{F2, F1}]), [E1], S),
    %% The external call E4 is included in the reply:
    {ok, _} = eval("(XXL)(Lin)(LC | m1)",
                   [{{D2,D1},[1,4]},{{D2,D3},[2]},{{D3,D1},[3]},
                    {{D4,D2},[5]},{{D4,D5},[7]},{{D5,D4},[6]}], S),
    %% The local call E1 is included in the reply:
    {ok, _} = eval("(XXL)(Lin)(XC | m1)", [{{D2,D1},[1,4]}], S),

    {ok, _} = eval(f("(LLin) (E | ~p || ~p) + (XLin) (E | ~p || ~p)",
                     [F2, F1, F2, F1]), [{E4,[1,4]}], S),

    {ok, _} = eval("# (ELin) E", 6, S),

    ok.

%% More test of Inter Call Graph, and regular expressions
lines(Conf) when is_list(Conf) ->
    S0 = new(),

    F1 = {m1,f1,1}, % X
    F2 = {m1,f2,2},
    F3 = {m1,f3,3},
    F4 = {m2,f4,4}, % X
    F5 = {m1,f5,5}, % X
    F6 = {m1,f6,6},

    E1 = {F1,F2},
    E2 = {F2,F1}, % X
    E3 = {F3,F2},
    E4 = {F1,F4}, % X
    E5 = {F2,F4}, % X
    E6 = {F5,F6},
    E7 = {F6,F4}, % X

    D1 = {F1,1},
    D2 = {F2,2},
    D3 = {F3,3},
    D4 = {F4,4},
    D5 = {F5,5},
    D6 = {F6,6},

    DefAt_m1 = [D1,D2,D3,D5,D6],
    X_m1 = [F1,F5],
    % L_m1 = [F2,F3,F6],
    XC_m1 = [E4,E5,E7],
    LC_m1 = [E1,E2,E3,E6],
    LCallAt_m1 = [{E1,1},{E3,3},{E6,6}],
    XCallAt_m1 = [{E2,2},{E4,4},{E5,5},{E7,7}],
    Info1 = #xref_mod{name = m1, app_name = [a1]},
    S1 = add_module(S0, Info1, DefAt_m1, X_m1, LCallAt_m1, XCallAt_m1,
                    XC_m1, LC_m1),

    DefAt_m2 = [D4],
    X_m2 = [F4],
    % L_m2 = [],
    XC_m2 = [],
    LC_m2 = [],
    LCallAt_m2 = [],
    XCallAt_m2 = [],
    Info2 = #xref_mod{name = m2, app_name = [a2]},
    S2 = add_module(S1, Info2, DefAt_m2, X_m2, LCallAt_m2, XCallAt_m2,
                    XC_m2, LC_m2),

    AppInfo1 = #xref_app{name = a1, rel_name = [r1]},
    S5 = add_application(S2, AppInfo1),
    AppInfo2 = #xref_app{name = a2, rel_name = [r1]},
    S6 = add_application(S5, AppInfo2),

    RelInfo = #xref_rel{name = r1},
    S7 = add_release(S6, RelInfo),

    S = set_up(S7),

    {ok, _} = eval("(XXL) (ELin) (EE | m1)",
                   [{{D1,D1},[1]},{{D1,D4},[1,4]},{{D3,D1},[3]},{{D3,D4},[3]},
                    {{D5,D4},[6]}], S),
    {ok, _} = eval("(XXL)(Lin) (E | m1)",
                   [{{D1,D2},[1]},{{D1,D4},[4]},{{D2,D1},[2]},
                    {{D2,D4},[5]},{{D3,D2},[3]},{{D5,D6},[6]},{{D6,D4},[7]}],
                   S),
    {ok, _} = eval("(E | m1) + (EE | m1)",
                   [E1,E2,E3,E4,E5,E6,E7,{F1,F1},{F3,F1},{F3,F4},{F5,F4}],
                   S),
    {ok, _} = eval("(Lin)(E | m1)",
                   [{E4,[4]},{E1,[1]},{E2,[2]},{E5,[5]},
                    {E3,[3]},{E7,[7]},{E6,[6]}], S),
    {ok, _} = eval("(ELin)(EE | m1)",
                   [{{F1,F1},[1]},{{F1,F4},[1,4]},{{F3,F1},[3]},{{F3,F4},[3]},
                    {{F5,F4},[6]}], S),
    {ok, _} = eval("(Lin)(E | m1) + (ELin)(EE | m1)",
                   [{E4,[1,4]},{E1,[1]},{E2,[2]},{E5,[5]},
                    {E3,[3]},{E7,[7]},{E6,[6]},
                    {{F1,F1},[1]},{{F3,F1},[3]},{{F3,F4},[3]},
                    {{F5,F4},[6]}], S),
    {ok, _} = eval("(Lin)(E | m1) - (ELin)(EE | m1)",
                   [{E1,[1]},{E2,[2]},{E5,[5]},
                    {E3,[3]},{E7,[7]},{E6,[6]}], S),
    {ok, _} = eval("(Lin)(E | m1) * (ELin)(EE | m1)",
                   [{E4,[4]}], S),
    {ok, _} = eval("(XXL)(Lin) (E | m1)",
                   [{{D1,D4},[4]},{{D1,D2},[1]},{{D2,D1},[2]},{{D2,D4},[5]},
                    {{D3,D2},[3]},{{D6,D4},[7]},{{D5,D6},[6]}], S),
    {ok, _} = eval("(XXL)(ELin) (EE | m1)",
                   [{{D1,D1},[1]},{{D1,D4},[1,4]},{{D3,D1},[3]},{{D3,D4},[3]},
                    {{D5,D4},[6]}], S),
    {ok, _} = eval("(XXL)(Lin)(Fun)(Lin) (E | m1)",
                   [{{D1,D4},[4]},{{D1,D2},[1]},{{D2,D1},[2]},{{D2,D4},[5]},
                    {{D3,D2},[3]},{{D6,D4},[7]},{{D5,D6},[6]}], S),
    {ok, _} = eval("(XXL)(ELin)(Fun)(ELin) (EE | m1)",
                   [{{D1,D1},[1]},{{D1,D4},[1,4]},{{D3,D1},[3]},{{D3,D4},[3]},
                    {{D5,D4},[6]}], S),

    %% A few tests on regexp.
    {ok, _} = eval("\"(foo\":Mod", parse_error, S),
    {ok, _} = eval("_Foo:_/_", parse_error, S),
    {ok, _} = eval("\".*foo\"", parse_error, S),
    {ok, _} = eval("_:_/_:Lin", parse_error, S),
    {ok, _} = eval("_:_/_:Mod", parse_error, S),
    {ok, _} = eval("_:_/_:App", parse_error, S),
    {ok, _} = eval("_:_/_:Rel", parse_error, S),
    {ok, _} = eval("m2:_/4", [F4], S),
    {ok, _} = eval("m2:_/4:Fun", [F4], S),
    {ok, _} = eval("\"m.?\":\"f.*\"/\"6\"", [F6], S),
    {ok, _} = eval("_:_/6", [F6], S),
    {ok, _} = eval("m1:\"f1\"/_", [F1], S),
    {ok, _} = eval("\"m1\":f1/_", [F1], S),
    {ok, _} = eval("\"m1\":Mod", [m1], S),
    {ok, _} = eval("\"a1\":App", [a1], S),
    {ok, _} = eval("\"r1\":Rel", [r1], S),
    {ok, _} = eval("_:_/-1", [], S),

    ok.

%% More Inter Call Graph, loops and "unusual" cases
loops(Conf) when is_list(Conf) ->
    S0 = new(),

    F1 = {m1,f1,1}, % X
    F2 = {m1,f2,2},
    F3 = {m1,f3,3}, % X
    F4 = {m1,f4,4},
    F5 = {m1,f5,5},
    F6 = {m1,f1,6}, % X
    F7 = {m1,f1,7},

    E1 = {F1,F1}, % X
    E2 = {F2,F2},
    E3 = {F3,F4},
    E4 = {F4,F5},
    E5 = {F5,F3}, % X

    D1 = {F1,1},
    D2 = {F2,2},
    D3 = {F3,3},
    D4 = {F4,4},
    D5 = {F5,5},
    D6 = {F6,6},
    D7 = {F7,7},
    DefAt_m1 = [D1,D2,D3,D4,D5,D6,D7],
    X_m1 = [F1,F3,F6],
    % L_m1 = [F2,F4,F5],
    XC_m1 = [],
    LC_m1 = [E1,E2,E3,E4,E5],
    LCallAt_m1 = [{E2,2},{E3,3},{E4,4}],
    XCallAt_m1 = [{E1,1},{E5,5}],
    Info1 = #xref_mod{name = m1, app_name = [a1]},
    S1 = add_module(S0, Info1, DefAt_m1, X_m1, LCallAt_m1, XCallAt_m1,
                    XC_m1, LC_m1),

    S = set_up(S1),

    % Neither F6 nor F7 is included. Perhaps one should change that?
    {ok, _} = eval("EE | m1", [E1,E2,{F3,F3}], S),
    {ok, _} = eval(f("(XXL)(ELin) (EE | ~p)", [F3]), [{{D3,D3},[3]}], S),

    {ok, _} = eval("m1->m1 | m1->m1", type_error, S),
    {ok, _} = eval(f("~p | ~p", [F2, F1]), type_error, S),

    {ok, _} = eval(f("range (closure EE | ~p)", [F1]), [F1], S),
    {ok, _} = eval(f("domain (closure EE || ~p)", [F3]), [F3], S),

    {ok, _} = eval(f("domain (closure E || ~p)", [F3]), [F3,F4,F5], S),

    {ok, _} = eval("components E", [[F1],[F2],[F3,F4,F5]], S),
    {ok, _} = eval("components EE", [[F1],[F2],[F3]], S),

    ok.

%% Simple tests when there is no data
no_data(Conf) when is_list(Conf) ->
    S0 = new(),
    S1 = set_up(S0),
    {ok, _} = eval("M", [], S1),
    {ok, _} = eval("A", [], S1),
    {ok, _} = eval("R", [], S1),

    ModInfo = #xref_mod{name = m, app_name = []},
    S2 = add_module(S1, ModInfo, [], [], [], [], [], []),
    AppInfo = #xref_app{name = a, rel_name = []},
    S3 = add_application(S2, AppInfo),
    RelInfo = #xref_rel{name = r, dir = ""},
    S4 = add_release(S3, RelInfo),
    S5 = set_up(S4),
    {ok, _} = eval("M", [m], S5),
    {ok, _} = eval("A", [a], S5),
    {ok, _} = eval("R", [r], S5),
    ok.

%% Modules mode
modules(Conf) when is_list(Conf) ->
    CopyDir = ?copydir,
    Dir = fname(CopyDir, "rel2"),
    X = fname(Dir, "x.erl"),
    Y = fname(Dir, "y.erl"),
    A1_1 = fname([Dir,"lib","app1-1.1"]),
    A2 = fname([Dir,"lib","app2-1.1"]),
    EB1_1 = fname(A1_1, "ebin"),
    EB2 = fname(A2, "ebin"),
    Xbeam = fname(EB2, "x.beam"),
    Ybeam = fname(EB1_1, "y.beam"),

    {ok, x} = compile:file(X, [debug_info, {outdir,EB2}]),
    {ok, y} = compile:file(Y, [debug_info, {outdir,EB1_1}]),

    {ok, S0} = xref_base:new([{xref_mode, modules}]),
    {ok, release2, S1} =
    xref_base:add_release(S0, Dir, [{name,release2}]),
    S = set_up(S1),
    {{error, _, {unavailable_analysis, undefined_function_calls}}, _} =
    xref_base:analyze(S, undefined_function_calls),
    {{error, _, {unavailable_analysis, locals_not_used}}, _} =
    xref_base:analyze(S, locals_not_used),
    {{error, _, {unavailable_analysis, {call, foo}}}, _} =
    xref_base:analyze(S, {call, foo}),
    {{error, _, {unavailable_analysis, {use, foo}}}, _} =
    xref_base:analyze(S, {use, foo}),
    analyze(undefined_functions, [{x,undef,0}], S),
    5 = length(xref_base:info(S)),

    %% More: all info, conversions.

    ok = file:delete(Xbeam),
    ok = file:delete(Ybeam),
    ok = xref_base:delete(S),
    ok.


%% Add modules, applications, releases, directories
add(Conf) when is_list(Conf) ->
    CopyDir = ?copydir,
    Dir = fname(CopyDir, "rel2"),
    UDir = fname([CopyDir,"dir","unreadable"]),
    DDir = fname(CopyDir,"dir"),
    UFile = fname([DDir, "dir","unreadable.beam"]),
    X = fname(Dir, "x.erl"),
    Y = fname(Dir, "y.erl"),
    A1_1 = fname([Dir,"lib","app1-1.1"]),
    A2 = fname([Dir,"lib","app2-1.1"]),
    EB1_1 = fname(A1_1, "ebin"),
    EB2 = fname(A2, "ebin"),
    Xbeam = fname(EB2, "x.beam"),
    Ybeam = fname(EB1_1, "y.beam"),

    {ok, x} = compile:file(X, [debug_info, {outdir,EB2}]),
    {ok, y} = compile:file(Y, [debug_info, {outdir,EB1_1}]),

    case os:type() of
        {unix, _} ->
            make_udir(UDir),
            make_ufile(UFile);
        _ ->
            true
    end,

    {error, _, {invalid_options,[not_an_option] }} =
    xref_base:new([not_an_option]),
    {error, _, {invalid_options,[{verbose,not_a_value}] }} =
    xref_base:new([{verbose,not_a_value}]),
    S = new(),
    {error, _, {invalid_options,[not_an_option]}} =
    xref_base:set_up(S, [not_an_option]),
    {error, _, {invalid_options,[{builtins,true},not_an_option]}} =
    xref_base:add_directory(S, foo, [{builtins,true},not_an_option]),
    {error, _, {invalid_options,[{builtins,not_a_value}]}} =
    xref_base:add_directory(S, foo, [{builtins,not_a_value}]),
    {error, _, {invalid_filename,{foo,bar}}} =
    xref_base:add_directory(S, {foo,bar}, []),
    {error, _, {invalid_options,[{builtins,true},not_an_option]}} =
    xref_base:add_module(S, foo, [{builtins,true},not_an_option]),
    {error, _, {invalid_options,[{builtins,not_a_value}]}} =
    xref_base:add_module(S, foo, [{builtins,not_a_value}]),
    {error, _, {invalid_filename,{foo,bar}}} =
    xref_base:add_module(S, {foo,bar}, []),
    {error, _, {invalid_options,[{builtins,true},not_an_option]}} =
    xref_base:add_application(S, foo, [{builtins,true},not_an_option]),
    {error, _, {invalid_options,[{builtins,not_a_value}]}} =
    xref_base:add_application(S, foo, [{builtins,not_a_value}]),
    {error, _, {invalid_filename,{foo,bar}}} =
    xref_base:add_application(S, {foo,bar}, []),
    {error, _, {invalid_options,[not_an_option]}} =
    xref_base:add_release(S, foo, [not_an_option]),
    {error, _, {invalid_options,[{builtins,not_a_value}]}} =
    xref_base:add_release(S, foo, [{builtins,not_a_value}]),
    {error, _, {invalid_filename,{foo,bar}}} =
    xref_base:add_release(S, {foo,bar}, []),
    {ok, S1} =
    xref_base:set_default(S, [{verbose,false}, {warnings, false}]),
    case os:type() of
        {unix, _} ->
            {error, _, {file_error, _, _}} =
            xref_base:add_release(S, UDir);
        _ ->
            true
    end,
    {error, _, {file_error, _, _}} =
    xref_base:add_release(S, fname(["/a/b/c/d/e/f","__foo"])),
    {ok, release2, S2} =
    xref_base:add_release(S1, Dir, [{name,release2}]),
    {error, _, {module_clash, {x, _, _}}} =
    xref_base:add_module(S2, Xbeam),
    {ok, S3} = xref_base:remove_release(S2, release2),
    {ok, rel2, S4} = xref_base:add_release(S3, Dir),
    {error, _, {release_clash, {rel2, _, _}}} =
    xref_base:add_release(S4, Dir),
    {ok, S5} = xref_base:remove_release(S4, rel2),
    %% One unreadable file and one JAM file found (no verification here):
    {ok, [], S6} = xref_base:add_directory(S5, fname(CopyDir,"dir"),
                                           [{recurse,true}, {warnings,true}]),
    case os:type() of
        {unix, _} ->
            {error, _, {file_error, _, _}} =
            xref_base:add_directory(S6, UDir);
        _ ->
            true
    end,
    {ok, app1, S7} = xref_base:add_application(S6, A1_1),
    {error, _, {application_clash, {app1, _, _}}} =
    xref_base:add_application(S7, A1_1),
    {ok, S8} = xref_base:remove_application(S7, app1),
    ok = xref_base:delete(S8),
    ok = file:delete(Xbeam),
    ok = file:delete(Ybeam),
    case os:type() of
        {unix, _} ->
            ok = file:del_dir(UDir),
            ok = file:delete(UFile);
        _ ->
            true
    end,
    ok.

%% Default values of options
default(Conf) when is_list(Conf) ->
    S = new(),
    {error, _, {invalid_options,[not_an_option]}} =
    xref_base:set_default(S, not_an_option, true),
    {error, _, {invalid_options,[{builtins, not_a_value}]}} =
    xref_base:set_default(S, builtins, not_a_value),
    {error, _, {invalid_options,[not_an_option]}} =
    xref_base:get_default(S, not_an_option),
    {error, _, {invalid_options,[not_an_option]}} =
    xref_base:set_default(S, [not_an_option]),

    D = xref_base:get_default(S),
    [{builtins,false},{recurse,false},{verbose,false},{warnings,true}] =
    D,

    ok = xref_base:delete(S),
    ok.

%% The info functions
info(Conf) when is_list(Conf) ->
    CopyDir = ?copydir,
    Dir = fname(CopyDir,"rel2"),
    LDir = fname(CopyDir,"lib_test"),
    X = fname(Dir, "x.erl"),
    Y = fname(Dir, "y.erl"),
    A1_1 = fname([Dir,"lib","app1-1.1"]),
    A2 = fname([Dir,"lib","app2-1.1"]),
    EB1_1 = fname(A1_1, "ebin"),
    EB2 = fname(A2, "ebin"),
    Xbeam = fname(EB2, "x.beam"),
    Ybeam = fname(EB1_1, "y.beam"),

    {ok, x} = compile:file(X, [debug_info, {outdir,EB2}]),
    {ok, y} = compile:file(Y, [debug_info, {outdir,EB1_1}]),

    {ok, _} = start(s),
    {error, _, {no_such_info, release}} = xref:info(s, release),
    {error, _, {no_such_info, release}} = xref:info(s, release, rel),
    {error, _, {no_such_module, mod}} = xref:info(s, modules, mod),
    {error, _, {no_such_application, app}} =
    xref:info(s, applications, app),
    {error, _, {no_such_release, rel}} = xref:info(s, releases, rel),
    ok = xref:set_default(s, [{verbose,false}, {warnings, false}]),
    {ok, rel2} = xref:add_release(s, Dir),
    9 = length(xref:info(s)),
    [{x,_}, {y, _}] = xref:info(s, modules),
    [{app1,_}, {app2, _}] = xref:info(s, applications),
    [{rel2,_}] = xref:info(s, releases),
    [] = xref:info(s, libraries),
    [{x,_}] = xref:info(s, modules, x),
    [{rel2,_}] = xref:info(s, releases, rel2),
    {error, _, {no_such_library, foo}} = xref:info(s, libraries, [foo]),

    {ok, lib1} =
    compile:file(fname(LDir,lib1),[debug_info,{outdir,LDir}]),
    {ok, lib2} =
    compile:file(fname(LDir,lib2),[debug_info,{outdir,LDir}]),
    ok = xref:set_library_path(s, [LDir], [{verbose,false}]),
    [{lib1,_}, {lib2, _}] = xref:info(s, libraries),
    [{lib1,_}, {lib2, _}] = xref:info(s, libraries, [lib1,lib2]),
    ok = file:delete(fname(LDir, "lib1.beam")),
    ok = file:delete(fname(LDir, "lib2.beam")),

    check_state(s),

    xref:stop(s),

    ok = file:delete(Xbeam),
    ok = file:delete(Ybeam),

    ok.

%% Library modules
lib(Conf) when is_list(Conf) ->
    CopyDir = ?copydir,
    Dir = fname(CopyDir,"lib_test"),
    UDir = fname([CopyDir,"dir","non_existent"]),

    {ok, lib1} = compile:file(fname(Dir,lib1),[debug_info,{outdir,Dir}]),
    {ok, lib2} = compile:file(fname(Dir,lib2),[debug_info,{outdir,Dir}]),
    {ok, lib3} = compile:file(fname(Dir,lib3),[debug_info,{outdir,Dir}]),
    {ok, t} = compile:file(fname(Dir,t),[debug_info,{outdir,Dir}]),

    {ok, _} = start(s),
    ok = xref:set_default(s, [{verbose,false}, {warnings, false}]),
    {ok, t} = xref:add_module(s, fname(Dir,"t.beam")),
    {error, _, {invalid_options,[not_an_option]}} =
    xref:set_library_path(s, ["foo"], [not_an_option]),
    {error, _, {invalid_path,otp}} = xref:set_library_path(s,otp),
    {error, _, {invalid_path,[""]}} = xref:set_library_path(s,[""]),
    {error, _, {invalid_path,[[$a | $b]]}} =
    xref:set_library_path(s,[[$a | $b]]),
    {error, _, {invalid_path,[otp]}} = xref:set_library_path(s,[otp]),
    {ok, []} = xref:get_library_path(s),
    ok = xref:set_library_path(s, [Dir], [{verbose,false}]),
    {ok, UnknownFunctions} = xref:q(s, "U"),
    [{lib1,unknown,0}, {lib2,local,0},
     {lib2,unknown,0}, {unknown,unknown,0}]
    = UnknownFunctions,
    {ok, [{lib2,f,0},{lib3,f,0}]} = xref:q(s, "DF"),
    {ok, []} = xref:q(s, "DF_1"),
    {ok, [{lib2,f,0}]} = xref:q(s, "DF_2"),
    {ok, [{lib2,f,0}]} = xref:q(s, "DF_3"),

    {ok, [unknown]} = xref:q(s, "UM"),
    {ok, UnknownDefAt} = xref:q(s, "(Lin)U"),
    [{{lib1,unknown,0},0},{{lib2,local,0},0}, {{lib2,unknown,0},0},
     {{unknown,unknown,0},0}] = UnknownDefAt,
    {ok, LibFuns} = xref:q(s, "X * LM"),
    [{lib2,f,0},{lib3,f,0}] = LibFuns,
    {ok, LibMods} = xref:q(s, "LM"),
    [lib1,lib2,lib3] = LibMods,
    {ok, [{{lib2,f,0},0},{{lib3,f,0},0}]} = xref:q(s, "(Lin) (LM * X)"),
    {ok, [{{lib1,unknown,0},0}, {{lib2,f,0},0}, {{lib2,local,0},0},
          {{lib2,unknown,0},0}, {{lib3,f,0},0}]} = xref:q(s,"(Lin)LM"),
    {ok,[lib1,lib2,lib3,t,unknown]} = xref:q(s,"M"),
    {ok,[{lib2,f,0},{lib3,f,0},{t,t,0}]} = xref:q(s,"X * M"),
    check_state(s),

    copy_file(fname(Dir, "lib1.erl"), fname(Dir,"lib1.beam")),
    ok = xref:set_library_path(s, [Dir]),
    {error, _, _} = xref:q(s, "U"),

    %% OTP-3921. AM and LM not always disjoint.
    {ok, lib1} = compile:file(fname(Dir,lib1),[debug_info,{outdir,Dir}]),
    {ok, lib1} = xref:add_module(s, fname(Dir,"lib1.beam")),
    check_state(s),

    {error, _, {file_error, _, _}} = xref:set_library_path(s, [UDir]),

    xref:stop(s),
    ok = file:delete(fname(Dir, "lib1.beam")),
    ok = file:delete(fname(Dir, "lib2.beam")),
    ok = file:delete(fname(Dir, "lib3.beam")),
    ok = file:delete(fname(Dir, "t.beam")),

    {ok, cp} = compile:file(fname(Dir,cp),[debug_info,{outdir,Dir}]),
    {ok, _} = start(s),
    ok = xref:set_default(s, [{verbose,false}, {warnings, false}]),
    {ok, cp} = xref:add_module(s, fname(Dir,"cp.beam")),
    {ok, [{lists, sort, 1}]} = xref:q(s, "U"),
    ok = xref:set_library_path(s, code_path),
    {ok, []} = xref:q(s, "U"),
    check_state(s),
    xref:stop(s),
    ok = file:delete(fname(Dir, "cp.beam")),
    ok.

%% Data read from the Abstract Code
read(Conf) when is_list(Conf) ->
    CopyDir = ?copydir,
    Dir = fname(CopyDir,"read"),
    File = fname(Dir, "read"),
    Beam = fname(Dir, "read.beam"),
    {ok, read} = compile:file(File, [debug_info,{outdir,Dir}]),
    do_read(File, abstract_v2),
    copy_file(fname(Dir, "read.beam.v1"), Beam),
    do_read(File, abstract_v1),
    ok = file:delete(Beam),
    ok.

do_read(File, Version) ->
    {ok, _} = start(s),
    ok = xref:set_default(s, [{verbose,false}, {warnings, false}]),
    {ok, read} = xref:add_module(s, File),

    {U, OK, OKB} = read_expected(Version),

    %% {ok, UC} = xref:q(s, "(Lin) UC"),
    %% RR = to_external(converse(family_to_relation(family(UC)))),
    %% lists:foreach(fun(X) -> io:format("~w~n", [X]) end, RR),
    Unres = to_external(relation_to_family(converse(from_term(U)))),
    {ok, Unres} =	xref:q(s, "(Lin) UC"),

    %% {ok, EE} = xref:q(s, "(Lin) (E - UC)"),
    %% AA = to_external(converse(family_to_relation(family(EE)))),
    %% lists:foreach(fun(X) -> io:format("~w~n", [X]) end, AA),
    Calls = to_external(relation_to_family(converse(from_term(OK)))),
    {ok, Calls} = xref:q(s, "(Lin) (E - UC) "),

    ok = check_state(s),
    {ok, UM} = xref:q(s, "UM"),
    true = member('$M_EXPR', UM),

    {ok, X} = xref:q(s, "X"),
    true = member({read, module_info, 0}, X),
    false = member({foo, module_info, 0}, X),
    false = member({erlang, module_info, 0}, X),
    {ok, Unknowns} = xref:q(s, "U"),
    false = member({read, module_info, 0}, Unknowns),
    true = member({foo, module_info, 0}, Unknowns),
    true = member({erlang, module_info, 0}, Unknowns),
    {ok, LC} = xref:q(s, "LC"),
    true = member({{read,bi,0},{read,bi,0}}, LC),

    ok = xref:set_library_path(s, add_erts_code_path(fname(code:lib_dir(kernel),ebin))),
    io:format("~p~n",[(catch xref:get_library_path(s))]),
    {ok, X2} = xref:q(s, "X"),
    ok = check_state(s),
    true = member({read, module_info, 0}, X2),
    false = member({foo, module_info, 0}, X2),
    true = member({erlang, module_info, 0}, X2),
    {ok, Unknowns2} = xref:q(s, "U"),
    false = member({read, module_info, 0}, Unknowns2),
    true = member({foo, module_info, 0}, Unknowns2),
    false = member({erlang, module_info, 0}, Unknowns2),

    ok = xref:remove_module(s, read),
    {ok, read} = xref:add_module(s, File, [{builtins,true}]),

    UnresB = to_external(relation_to_family(converse(from_term(U)))),
    {ok, UnresB} = xref:q(s, "(Lin) UC"),
    CallsB = to_external(relation_to_family(converse(from_term(OKB)))),
    {ok, CallsB} = xref:q(s, "(Lin) (E - UC) "),
    ok = check_state(s),
    {ok, XU} = xref:q(s, "XU"),
    Erl = set([{erlang,length,1},{erlang,integer,1},
               {erlang,binary_to_term,1}]),
    [{erlang,binary_to_term,1},{erlang,length,1}] =
    to_external(intersection(set(XU), Erl)),
    xref:stop(s).

%% What is expected when xref_SUITE_data/read/read.erl is added:
read_expected(Version) ->
    %% Line positions in xref_SUITE_data/read/read.erl:
    POS1 = 28, POS2 = POS1+10, POS3 = POS2+6, POS4 = POS3+6, POS5 = POS4+10,
    POS6 = POS5+5, POS7 = POS6+6, POS8 = POS7+6, POS9 = POS8+8,
    POS10 = POS9+10, POS11 = POS10+7, POS12 = POS11+8, POS13 = POS12+10,
    POS14 = POS13+18, POS15 = POS14+23,

    FF = {read,funfuns,0},
    U = [{POS1+5,{FF,{dist,'$F_EXPR',0}}},
         {POS1+8,{FF,{dist,'$F_EXPR',0}}},
         {POS2+8,{{read,funfuns,0},{expr,'$F_EXPR',1}}},
         {POS3+4,{FF,{expr,'$F_EXPR',2}}},
         {POS4+2,{FF,{modul,'$F_EXPR',1}}},
         {POS4+4,{FF,{spm,'$F_EXPR',1}}},
         {POS4+6,{FF,{spm,'$F_EXPR',1}}},
         {POS4+8,{FF,{spm,'$F_EXPR',1}}},
         {POS5+1,{FF,{'$M_EXPR','$F_EXPR',0}}},
         {POS5+2,{FF,{'$M_EXPR','$F_EXPR',0}}},
         {POS5+3,{FF,{'$M_EXPR','$F_EXPR',0}}},
         {POS6+1,{FF,{'$M_EXPR','$F_EXPR',0}}},
         {POS6+2,{FF,{'$M_EXPR','$F_EXPR',0}}},
         {POS6+4,{FF,{n,'$F_EXPR',-1}}},
         {POS7+1,{FF,{'$M_EXPR',f,1}}},
         {POS7+2,{FF,{'$M_EXPR',f,1}}},
         {POS8+2,{FF,{hej,'$F_EXPR',1}}},
         {POS8+3,{FF,{t,'$F_EXPR',1}}},
         {POS8+5,{FF,{a,'$F_EXPR',1}}},
         {POS8+7,{FF,{m,'$F_EXPR',1}}},
         {POS9+1,{FF,{'$M_EXPR',f,1}}},
         {POS9+3,{FF,{a,'$F_EXPR',1}}},
         {POS10+1,{FF,{'$M_EXPR',foo,1}}},
         {POS10+2,{FF,{'$M_EXPR','$F_EXPR',1}}},
         {POS10+3,{FF,{'$M_EXPR','$F_EXPR',2}}},
         {POS10+4,{FF,{'$M_EXPR','$F_EXPR',1}}},
         {POS10+5,{FF,{'$M_EXPR',san,1}}},
         {POS10+6,{FF,{'$M_EXPR','$F_EXPR',1}}},
         {POS11+1,{FF,{'$M_EXPR','$F_EXPR',1}}},
         {POS11+2,{FF,{'$M_EXPR','$F_EXPR',-1}}},
         {POS11+3,{FF,{m,f,-1}}},
         {POS11+4,{FF,{m,f,-1}}},
         {POS11+5,{FF,{'$M_EXPR','$F_EXPR',1}}},
         {POS11+6,{FF,{'$M_EXPR','$F_EXPR',1}}},
         {POS12+1,{FF,{'$M_EXPR','$F_EXPR',-1}}},
         {POS12+4,{FF,{'$M_EXPR','$F_EXPR',2}}},
         {POS12+7,{FF,{'$M_EXPR','$F_EXPR',-1}}},
         {POS12+8,{FF,{m4,f4,-1}}},
         {POS13+2,{FF,{debug,'$F_EXPR',0}}},
         {POS13+3,{FF,{'$M_EXPR','$F_EXPR',-1}}},
         {POS14+8,{{read,bi,0},{'$M_EXPR','$F_EXPR',1}}}],

    O1 = [{20,{{read,lc,0},{ets,new,0}}},
          {21,{{read,lc,0},{ets,tab2list,1}}},
          {POS1+1,{FF,{erlang,spawn,1}}},
          {POS1+1,{FF,{mod17,fun17,0}}},
          {POS1+2,{FF,{erlang,spawn,1}}},
          {POS1+2,{FF,{read,local,0}}},
          {POS1+3,{FF,{erlang,spawn,1}}},
          {POS1+4,{FF,{dist,func,0}}},
          {POS1+4,{FF,{erlang,spawn,1}}},
          {POS1+5,{FF,{erlang,spawn,1}}},
          {POS1+6,{FF,{erlang,spawn_link,1}}},
          {POS1+6,{FF,{mod17,fun17,0}}},
          {POS1+7,{FF,{dist,func,0}}},
          {POS1+7,{FF,{erlang,spawn_link,1}}},
          {POS1+8,{FF,{erlang,spawn_link,1}}},
          {POS2+1,{FF,{d,f,0}}},
          {POS2+1,{FF,{dist,func,2}}},
          {POS2+1,{FF,{erlang,spawn,2}}},
          {POS2+2,{FF,{dist,func,2}}},
          {POS2+2,{FF,{erlang,spawn,2}}},
          {POS2+2,{FF,{mod42,func,0}}},
          {POS2+3,{FF,{d,f,0}}},
          {POS2+3,{FF,{dist,func,2}}},
          {POS2+3,{FF,{erlang,spawn_link,2}}},
          {POS2+4,{FF,{dist,func,2}}},
          {POS2+4,{FF,{erlang,spawn_link,2}}},
          {POS2+4,{FF,{mod42,func,0}}},
          {POS3+1,{FF,{dist,func,2}}},
          {POS3+3,{FF,{dist,func,2}}},
          {POS4+1,{FF,{erlang,spawn,4}}},
          {POS4+1,{FF,{modul,function,0}}},
          {POS4+2,{FF,{erlang,spawn,4}}},
          {POS4+3,{FF,{dist,func,2}}},
          {POS4+3,{FF,{erlang,spawn,4}}},
          {POS4+3,{FF,{spm,spf,2}}},
          {POS4+4,{FF,{dist,func,2}}},
          {POS4+4,{FF,{erlang,spawn,4}}},
          {POS4+5,{FF,{dist,func,2}}},
          {POS4+5,{FF,{erlang,spawn_link,4}}},
          {POS4+5,{FF,{spm,spf,2}}},
          {POS4+6,{FF,{dist,func,2}}},
          {POS4+6,{FF,{erlang,spawn_link,4}}},
          {POS4+7,{FF,{erlang,spawn_opt,4}}},
          {POS4+7,{FF,{read,bi,0}}},
          {POS4+7,{FF,{spm,spf,2}}},
          {POS4+8,{FF,{erlang,spawn_opt,4}}},
          {POS4+8,{FF,{read,bi,0}}},
          {POS5+1,{FF,{erlang,spawn,1}}},
          {POS5+2,{FF,{erlang,spawn,1}}},
          {POS5+3,{FF,{erlang,spawn_link,1}}},
          {POS6+1,{FF,{erlang,spawn,2}}},
          {POS6+2,{FF,{erlang,spawn_link,2}}},
          {POS7+1,{FF,{erlang,spawn,4}}},
          {POS7+2,{FF,{erlang,spawn_opt,4}}},
          {POS8+1,{FF,{hej,san,1}}},
          {POS8+4,{FF,{a,b,1}}},
          {POS8+4,{FF,{erlang,apply,2}}},
          {POS8+5,{FF,{erlang,apply,2}}},
          {POS8+6,{FF,{m,f,1}}},
          {POS9+1,{FF,{read,bi,0}}},
          {POS9+2,{FF,{a,b,1}}},
          {POS9+2,{FF,{erlang,apply,2}}},
          {POS9+3,{FF,{erlang,apply,2}}},
          {POS9+4,{FF,{erlang,apply,2}}},
          {POS9+4,{FF,{erlang,not_a_function,1}}},
          {POS9+5,{FF,{mod,func,2}}},
          {POS9+6,{FF,{erlang,apply,1}}},
          {POS9+7,{FF,{erlang,apply,2}}},
          {POS9+7,{FF,{math,add3,1}}},
          {POS9+8,{FF,{q,f,1}}},
          {POS10+4,{FF,{erlang,apply,2}}},
          {POS10+5,{FF,{mod1,fun1,1}}},
          {POS11+6,{FF,{erlang,apply,2}}},
          {POS12+1,{FF,{erlang,apply,2}}},
          {POS12+4,{FF,{erlang,apply,2}}},
          {POS12+5,{FF,{m3,f3,2}}},
          {POS12+7,{FF,{erlang,apply,2}}},
          {POS13+1,{FF,{dm,df,1}}},
          {POS13+6,{{read,bi,0},{foo,module_info,0}}},
          {POS13+7,{{read,bi,0},{read,module_info,0}}},
          {POS13+9,{{read,bi,0},{t,foo,1}}},
          {POS14+11,{{read,bi,0},{erlang,module_info,0}}},
          {POS14+17,{{read,bi,0},{read,bi,0}}}],

    OK = case Version of
             abstract_v1 ->
                 [{0,{FF,{read,'$F_EXPR',178}}},
                  {0,{FF,{modul,'$F_EXPR',179}}}]
                 ++ O1;
             _ ->
                 [{16,{FF,{read,'$F_EXPR',178}}},
                  {17,{FF,{modul,'$F_EXPR',179}}}]
                 ++
                 O1
         end,

    %% When builtins =:= true:
    OKB1 = [{POS13+1,{FF,{erts_debug,apply,4}}},
            {POS13+2,{FF,{erts_debug,apply,4}}},
            {POS13+3,{FF,{erts_debug,apply,4}}},
            {POS1+3, {FF,{erlang,binary_to_term,1}}},
            {POS3+1, {FF,{erlang,spawn,3}}},
            {POS3+2, {FF,{erlang,spawn,3}}},
            {POS3+3,  {FF,{erlang,spawn_link,3}}},
            {POS3+4, {FF,{erlang,spawn_link,3}}},
            {POS6+4, {FF,{erlang,spawn,3}}},
            {POS8+6,{FF,{erlang,apply,3}}},
            {POS8+7,{FF,{erlang,apply,3}}},
            {POS9+1,{FF,{erlang,apply,3}}},
            {POS9+5,{FF,{erlang,apply,3}}},
            {POS11+1,{FF,{erlang,apply,3}}},
            {POS11+2,{FF,{erlang,apply,3}}},
            {POS11+3,{FF,{erlang,apply,3}}},
            {POS11+4,{FF,{erlang,apply,3}}},
            {POS12+5,{FF,{erlang,apply,3}}},
            {POS12+8,{FF,{erlang,apply,3}}},
            {POS13+5, {{read,bi,0},{erlang,length,1}}},
            {POS14+3, {{read,bi,0},{erlang,length,1}}}],

    %% Operators (OTP-8647):
    OKB = case Version of
              abstract_v1 ->
                  [{POS8+3, {FF,{erlang,apply,3}}},
                   {POS10+1, {FF,{erlang,apply,3}}},
                   {POS10+6, {FF,{erlang,apply,3}}}];
              _ ->
                  [{POS13+16, {{read,bi,0},{erlang,'!',2}}},
                   {POS13+16, {{read,bi,0},{erlang,'-',1}}},
                   {POS13+16, {{read,bi,0},{erlang,self,0}}},
                   {POS15+1,  {{read,bi,0},{erlang,'>',2}}},
                   {POS15+2,  {{read,bi,0},{erlang,'-',2}}},
                   {POS15+2,  {{read,bi,0},{erlang,'*',2}}},
                   {POS15+8,  {{read,bi,0},{erlang,'/',2}}}]
          end
    ++ [{POS14+19, {{read,bi,0},{erlang,'+',2}}},
        {POS14+21, {{read,bi,0},{erlang,'+',2}}},
        {POS13+16, {{read,bi,0},{erlang,'==',2}}},
        {POS14+15, {{read,bi,0},{erlang,'==',2}}},
        {POS13+5,  {{read,bi,0},{erlang,'>',2}}},
        {POS14+3,  {{read,bi,0},{erlang,'>',2}}}]
    ++ OKB1 ++ OK,

    {U, OK, OKB}.

%% Data read from the Abstract Code (cont)
read2(Conf) when is_list(Conf) ->
    %% Handles the spawn_opt versions added in R9 (OTP-4180).
    %% Expected augmentations: try/catch, cond.
    CopyDir = ?copydir,
    Dir = fname(CopyDir,"read"),
    File = fname(Dir, "read2.erl"),
    MFile = fname(Dir, "read2"),
    Beam = fname(Dir, "read2.beam"),
    Test = <<"-module(read2).
              -compile(export_all).

              f() ->
                  spawn_opt({read2,f}, % POS2
                            [f()]),
                  spawn_opt(fun() -> foo end, [link]),
                  spawn_opt(f(),
                            {read2,f}, [{min_heap_size,1000}]),
                  spawn_opt(f(),
                            fun() -> f() end, [flopp]),
                  spawn_opt(f(),
                            read2, f, [], []);
              f() ->
                  %% Duplicated unresolved calls are ignored:
                  (f())(foo,bar),(f())(foo,bar). % POS1

              %% Warning forms must be ignored.
              -warning(must_not_crash).
             ">>,
    ok = file:write_file(File, Test),
    {ok, read2} = compile:file(File, [debug_info,{outdir,Dir}]),

    {ok, _} = xref:start(s),
    {ok, read2} = xref:add_module(s, MFile),
    {U0, OK0} = read2_expected(),

    U = to_external(relation_to_family(converse(from_term(U0)))),
    OK = to_external(relation_to_family(converse(from_term(OK0)))),
    {ok, U2} = xref:q(s, "(Lin) UC"),
    {ok, OK2} = xref:q(s, "(Lin) (E - UC)"),
    true = U =:= U2,
    true = OK =:= OK2,
    ok = check_state(s),
    xref:stop(s),

    ok = file:delete(File),
    ok = file:delete(Beam),
    ok.


read2_expected() ->
    POS1 = 16,
    POS2 = 5,
    FF = {read2,f,0},
    U =  [{POS1,{FF,{'$M_EXPR','$F_EXPR',2}}}],
    OK = [{POS2,{FF,{erlang,spawn_opt,2}}},
          {POS2,{FF,FF}},
          {POS2+1,{FF,FF}},
          {POS2+2,{FF,{erlang,spawn_opt,2}}},
          {POS2+3,{FF,{erlang,spawn_opt,3}}},
          {POS2+3,{FF,FF}},
          {POS2+3,{FF,FF}},
          {POS2+5,{FF,{erlang,spawn_opt,3}}},
          {POS2+5,{FF,FF}},
          {POS2+6,{FF,FF}},
          {POS2+7,{FF,{erlang,spawn_opt,5}}},
          {POS2+7,{FF,FF}},
          {POS2+7,{FF,FF}},
          {POS1,{FF,FF}}],
    {U, OK}.

%% Remove modules, applications, releases
remove(Conf) when is_list(Conf) ->
    S = new(),
    {error, _, {no_such_module, mod}} =
    xref_base:remove_module(S, mod),
    {error, _, {no_such_application, app}} =
    xref_base:remove_application(S, app),
    {error, _, {no_such_release, rel}} =
    xref_base:remove_release(S, rel),
    ok = xref_base:delete(S),
    ok.

%% Replace modules, applications, releases
replace(Conf) when is_list(Conf) ->
    CopyDir = ?copydir,
    Dir = fname(CopyDir,"rel2"),
    X = fname(Dir, "x.erl"),
    Y = fname(Dir, "y.erl"),
    A1_0 = fname(Dir, fname("lib","app1-1.0")),
    A1_1 = fname(Dir, fname("lib","app1-1.1")),
    A2 = fname(Dir, fname("lib","app2-1.1")),
    EB1_0 = fname(A1_0, "ebin"),
    EB1_1 = fname(A1_1, "ebin"),
    Xbeam = fname(EB1_1, "x.beam"),
    Ybeam = fname(EB1_1, "y.beam"),

    {ok, x} = compile:file(X, [debug_info, {outdir,EB1_0}]),
    {ok, x} = compile:file(X, [debug_info, {outdir,EB1_1}]),
    {ok, y} = compile:file(Y, [debug_info, {outdir,EB1_1}]),

    {ok, _} = start(s),
    {ok, false} = xref:set_default(s, verbose, false),
    {ok, true} = xref:set_default(s, warnings, false),
    {ok, rel2} = xref:add_release(s, Dir, []),
    {error, _, _} = xref:replace_application(s, app1, "no_data"),
    {error, _, {no_such_application, app12}} =
    xref:replace_application(s, app12, A1_0, []),
    {error, _, {invalid_filename,{foo,bar}}} =
    xref:replace_application(s, app1, {foo,bar}, []),
    {error, _, {invalid_options,[not_an_option]}} =
    xref:replace_application(s, foo, bar, [not_an_option]),
    {error, _, {invalid_options,[{builtins,not_a_value}]}} =
    xref:replace_application(s, foo, bar, [{builtins,not_a_value}]),
    {ok, app1} =
    xref:replace_application(s, app1, A1_0),
    [{_, AppInfo}] = xref:info(s, applications, app1),
    {value, {release, [rel2]}} = keysearch(release, 1, AppInfo),

    {error, _, {no_such_module, xx}} =
    xref:replace_module(s, xx, Xbeam, []),
    {error, _, {invalid_options,[{builtins,true},not_an_option]}} =
    xref:replace_module(s, foo, bar,[{builtins,true},not_an_option]),
    {error, _, {invalid_options,[{builtins,not_a_value}]}} =
    xref:replace_module(s, foo, bar, [{builtins,not_a_value}]),
    {error, _, {invalid_filename,{foo,bar}}} =
    xref:replace_module(s, x, {foo,bar}),
    {ok, x} = xref:replace_module(s, x, Xbeam),
    [{x, ModInfo}] = xref:info(s, modules, x),
    {value, {application, [app1]}} =
    keysearch(application, 1, ModInfo),

    {ok, x} = compile:file(X, [no_debug_info, {outdir,EB1_1}]),
    {error, _, {no_debug_info, _}} = xref:replace_module(s, x, Xbeam),
    {error, _, {module_mismatch, x,y}} =
    xref:replace_module(s, x, Ybeam),
    case os:type() of
        {unix, _} ->
            hide_file(Ybeam),
            {error, _, {file_error, _, _}} =
            xref:replace_module(s, x, Ybeam);
        _ ->
            true
    end,
    ok = xref:remove_module(s, x),
    {error, _, {no_debug_info, _}} = xref:add_module(s, Xbeam),

    %% "app2" is ignored, the old application name is kept
    {ok, app1} = xref:replace_application(s, app1, A2),

    xref:stop(s),
    ok = file:delete(fname(EB1_0, "x.beam")),
    ok = file:delete(Xbeam),
    ok = file:delete(Ybeam),
    ok.

%% The update() function
update(Conf) when is_list(Conf) ->
    CopyDir = ?copydir,
    Dir = fname(CopyDir,"update"),
    Source = fname(Dir, "x.erl"),
    Beam = fname(Dir, "x.beam"),
    copy_file(fname(Dir, "x.erl.1"), Source),
    {ok, x} = compile:file(Source, [debug_info, {outdir,Dir}]),

    {ok, _} = start(s),
    ok = xref:set_default(s, [{verbose,false}, {warnings, false}]),
    {ok, [x]} = xref:add_directory(s, Dir, [{builtins,true}]),
    {error, _, {invalid_options,[not_an_option]}} =
    xref:update(s, [not_an_option]),
    {ok, []} = xref:update(s),
    {ok, [{erlang,atom_to_list,1}]} = xref:q(s, "XU"),

    [{x, ModInfo}] = xref:info(s, modules, x),
    case keysearch(directory, 1, ModInfo) of
        {value, {directory, Dir}} -> ok
    end,

    timer:sleep(2000), % make sure modification time has changed
    copy_file(fname(Dir, "x.erl.2"), Source),
    {ok, x} = compile:file(Source, [debug_info, {outdir,Dir}]),
    {ok, [x]} = xref:update(s, []),
    {ok, [{erlang,list_to_atom,1}]} = xref:q(s, "XU"),

    timer:sleep(2000),
    {ok, x} = compile:file(Source, [no_debug_info,{outdir,Dir}]),
    {error, _, {no_debug_info, _}} = xref:update(s),

    xref:stop(s),
    ok = file:delete(Beam),
    ok = file:delete(Source),
    ok.

%% OTP-4695: Deprecated functions.
deprecated(Conf) when is_list(Conf) ->
    Dir = ?copydir,
    File = fname(Dir, "depr.erl"),
    MFile_r9c = fname(Dir, "depr_r9c"),
    MFile = fname(Dir, "depr"),
    Beam = fname(Dir, "depr.beam"),
    %% This file has been compiled to ?datadir/depr_r9c.beam
    %% using the R9C compiler. From R10B and onwards the linter
    %% checks the 'deprecated' attribute as well.
    %     Test = <<"-module(depr).

    %               -export([t/0,f/1,bar/2,f/2,g/3]).

    %               -deprecated([{f,1},                             % DF
    %                            {bar,2,eventually}]).              % DF_3
    %               -deprecated([{f,1,next_major_release}]).        % DF_2 (again)
    %               -deprecated([{frutt,0,next_version}]).          % message...
    %               -deprecated([{f,2,next_major_release},          % DF_2
    %                            {g,3,next_version},                % DF_1
    %                            {ignored,10,100}]).                % message...
    %               -deprecated([{does_not_exist,1}]).              % message...

    %               -deprecated(foo).                               % message...

    %               t() ->
    %                   frutt(1),
    %                   g(1,2, 3),
    %                   ?MODULE:f(10).

    %               f(A) ->
    %                   ?MODULE:f(A,A).

    %               f(X, Y) ->
    %                   ?MODULE:g(X, Y, X).

    %               g(F, G, H) ->
    %                   ?MODULE:bar(F, {G,H}).

    %               bar(_, _) ->
    %                   true.

    %               frutt(_) ->
    %                   frutt().

    %               frutt() ->
    %                   true.
    %              ">>,

    %    ok = file:write_file(File, Test),
    %    {ok, depr_r9c} = compile:file(File, [debug_info,{outdir,Dir}]),

    {ok, _} = xref:start(s),
    {ok, depr_r9c} = xref:add_module(s, MFile_r9c),
    M9 = depr_r9c,
    DF_1 = usort([{{M9,f,2},{M9,g,3}}]),
    DF_2 = usort(DF_1++[{{M9,f,1},{M9,f,2}},{{M9,t,0},{M9,f,1}}]),
    DF_3 = usort(DF_2++[{{M9,g,3},{M9,bar,2}}]),
    DF = usort(DF_3++[{{M9,t,0},{M9,f,1}}]),

    {ok,DF} = xref:analyze(s, deprecated_function_calls),
    {ok,DF_1} =
    xref:analyze(s, {deprecated_function_calls,next_version}),
    {ok,DF_2} =
    xref:analyze(s, {deprecated_function_calls,next_major_release}),
    {ok,DF_3} =
    xref:analyze(s, {deprecated_function_calls,eventually}),

    D = to_external(range(from_term(DF))),
    D_1 = to_external(range(from_term(DF_1))),
    D_2 = to_external(range(from_term(DF_2))),
    D_3 = to_external(range(from_term(DF_3))),

    {ok,D} = xref:analyze(s, deprecated_functions),
    {ok,D_1} =
    xref:analyze(s, {deprecated_functions,next_version}),
    {ok,D_2} =
    xref:analyze(s, {deprecated_functions,next_major_release}),
    {ok,D_3} =
    xref:analyze(s, {deprecated_functions,eventually}),

    ok = check_state(s),
    xref:stop(s),

    Test2= <<"-module(depr).

              -export([t/0,f/1,bar/2,f/2,g/3]).

              -deprecated([{'_','_',eventually}]).            % DF_3
              -deprecated([{f,'_',next_major_release}]).      % DF_2
              -deprecated([{g,'_',next_version}]).            % DF_1
              -deprecated([{bar,2}]).                         % DF

              t() ->
                  g(1,2, 3),
                  ?MODULE:f(10).

              f(A) ->
                  ?MODULE:f(A,A).

              f(X, Y) ->
                  ?MODULE:g(X, Y, X).

              g(F, G, H) ->
                  ?MODULE:bar(F, {G,H}).

              bar(_, _) ->
                  ?MODULE:t().
             ">>,

    ok = file:write_file(File, Test2),
    {ok, depr} = compile:file(File, [debug_info,{outdir,Dir}]),

    {ok, _} = xref:start(s),
    {ok, depr} = xref:add_module(s, MFile),

    M = depr,
    DFa_1 = usort([{{M,f,2},{M,g,3}}]),
    DFa_2 = usort(DFa_1++[{{M,f,1},{M,f,2}},{{M,t,0},{M,f,1}}]),
    DFa_3 = usort(DFa_2++[{{M,bar,2},{M,t,0}},{{M,g,3},{M,bar,2}}]),
    DFa = DFa_3,

    {ok,DFa} = xref:analyze(s, deprecated_function_calls),
    {ok,DFa_1} =
    xref:analyze(s, {deprecated_function_calls,next_version}),
    {ok,DFa_2} =
    xref:analyze(s, {deprecated_function_calls,next_major_release}),
    {ok,DFa_3} =
    xref:analyze(s, {deprecated_function_calls,eventually}),

    ok = check_state(s),
    xref:stop(s),

    %% All of the module is deprecated.
    Test3= <<"-module(depr).

              -export([t/0,f/1,bar/2,f/2,g/3]).

              -deprecated([{f,'_',next_major_release}]).      % DF_2
              -deprecated([{g,'_',next_version}]).            % DF_1
              -deprecated(module).                            % DF

              t() ->
                  g(1,2, 3),
                  ?MODULE:f(10).

              f(A) ->
                  ?MODULE:f(A,A).

              f(X, Y) ->
                  ?MODULE:g(X, Y, X).

              g(F, G, H) ->
                  ?MODULE:bar(F, {G,H}).

              bar(_, _) ->
                  ?MODULE:t().
             ">>,

    ok = file:write_file(File, Test3),
    {ok, depr} = compile:file(File, [debug_info,{outdir,Dir}]),

    {ok, _} = xref:start(s),
    {ok, depr} = xref:add_module(s, MFile),

    DFb_1 = usort([{{M,f,2},{M,g,3}}]),
    DFb_2 = usort(DFb_1++[{{M,f,1},{M,f,2}},{{M,t,0},{M,f,1}}]),
    DFb_3 = DFb_2,
    DFb = usort(DFb_2++[{{M,bar,2},{M,t,0}},{{M,g,3},{M,bar,2}}]),

    {ok,DFb} = xref:analyze(s, deprecated_function_calls),
    {ok,DFb_1} =
    xref:analyze(s, {deprecated_function_calls,next_version}),
    {ok,DFb_2} =
    xref:analyze(s, {deprecated_function_calls,next_major_release}),
    {ok,DFb_3} =
    xref:analyze(s, {deprecated_function_calls,eventually}),

    ok = check_state(s),
    xref:stop(s),

    ok = file:delete(File),
    ok = file:delete(Beam),
    ok.


%% OTP-5152: try/catch, final (?) version.
trycatch(Conf) when is_list(Conf) ->
    Dir = ?copydir,
    File = fname(Dir, "trycatch.erl"),
    MFile = fname(Dir, "trycatch"),
    Beam = fname(Dir, "trycatch.beam"),
    Test = <<"-module(trycatch).

              -export([trycatch/0]).

              trycatch() ->
                  try
                     foo:bar(),
                     bar:foo() of
                        1 -> foo:foo();
                        2 -> bar:bar()
                  catch
                     error:a -> err:e1();
                     error:b -> err:e2()
                  after
                     fini:shed()
                  end.
             ">>,

    ok = file:write_file(File, Test),
    {ok, trycatch} = compile:file(File, [debug_info,{outdir,Dir}]),

    {ok, _} = xref:start(s),
    {ok, trycatch} = xref:add_module(s, MFile),
    A = trycatch,
    {ok,[{{{A,A,0},{bar,bar,0}},[10]},
         {{{A,A,0},{bar,foo,0}},[8]},
         {{{A,A,0},{err,e1,0}},[12]},
         {{{A,A,0},{err,e2,0}},[13]},
         {{{A,A,0},{fini,shed,0}},[15]},
         {{{A,A,0},{foo,bar,0}},[7]},
         {{{A,A,0},{foo,foo,0}},[9]}]} =
    xref:q(s, "(Lin) (E | trycatch:trycatch/0)"),

    ok = check_state(s),
    xref:stop(s),

    ok = file:delete(File),
    ok = file:delete(Beam),
    ok.


%% OTP-5653: fun M:F/A.
fun_mfa(Conf) when is_list(Conf) ->
    Dir = ?copydir,
    File = fname(Dir, "fun_mfa.erl"),
    MFile = fname(Dir, "fun_mfa"),
    Beam = fname(Dir, "fun_mfa.beam"),
    Test = <<"-module(fun_mfa).

              -export([t/0, t1/0, t2/0, t3/0]).

              t() ->
                  F = fun ?MODULE:t/0,
                  (F)().

              t1() ->
                  F = fun t/0,
                  (F)().

              t2() ->
                  fun ?MODULE:t/0().

              t3() ->
                  fun t3/0().
             ">>,

    ok = file:write_file(File, Test),
    A = fun_mfa,
    {ok, A} = compile:file(File, [debug_info,{outdir,Dir}]),
    {ok, _} = xref:start(s),
    {ok, A} = xref:add_module(s, MFile, {warnings,false}),
    {ok, [{{{A,t,0},{'$M_EXPR','$F_EXPR',0}},[7]},
          {{{A,t,0},{A,t,0}},[6]},
          {{{A,t1,0},{'$M_EXPR','$F_EXPR',0}},[11]},
          {{{A,t1,0},{A,t,0}},[10]},
          {{{A,t2,0},{A,t,0}},[14]},
          {{{A,t3,0},{fun_mfa,t3,0}},[17]}]} =
    xref:q(s, "(Lin) E"),

    ok = check_state(s),
    xref:stop(s),

    ok = file:delete(File),
    ok = file:delete(Beam),
    ok.

%% Same as the previous test case, except that we use a BEAM file
%% that was compiled by an R14 compiler to test backward compatibility.
fun_mfa_r14(Conf) when is_list(Conf) ->
    Dir = proplists:get_value(data_dir, Conf),
    MFile = fname(Dir, "fun_mfa_r14"),

    A = fun_mfa_r14,
    {ok, _} = xref:start(s),
    {ok, A} = xref:add_module(s, MFile, {warnings,false}),
    {ok, [{{{A,t,0},{'$M_EXPR','$F_EXPR',0}},[7]},
          {{{A,t,0},{A,t,0}},[6]},
          {{{A,t1,0},{'$M_EXPR','$F_EXPR',0}},[11]},
          {{{A,t1,0},{A,t,0}},[10]},
          {{{A,t2,0},{A,t,0}},[14]},
          {{{A,t3,0},{A,t3,0}},[17]}]} =
    xref:q(s, "(Lin) E"),

    ok = check_state(s),
    xref:stop(s),

    ok.

%% fun M:F/A with varibles.
fun_mfa_vars(Conf) when is_list(Conf) ->
    Dir = ?copydir,
    File = fname(Dir, "fun_mfa_vars.erl"),
    MFile = fname(Dir, "fun_mfa_vars"),
    Beam = fname(Dir, "fun_mfa_vars.beam"),
    Test = <<"-module(fun_mfa_vars).

              -export([t/1, t1/1, t2/3]).

              t(Mod) ->
                  F = fun Mod:bar/2,
                  (F)(a, b).

              t1(Name) ->
                  F = fun ?MODULE:Name/1,
                  (F)(a).

              t2(Mod, Name, Arity) ->
                  F = fun Mod:Name/Arity,
                  (F)(a).

              t3(Arity) ->
                  F = fun ?MODULE:t/Arity,
                  (F)(1, 2, 3).

              t4(Mod, Name) ->
                  F = fun Mod:Name/3,
                  (F)(a, b, c).

              t5(Mod, Arity) ->
                  F = fun Mod:t/Arity,
                  (F)().
             ">>,

    ok = file:write_file(File, Test),
    A = fun_mfa_vars,
    {ok, A} = compile:file(File, [report,debug_info,{outdir,Dir}]),
    {ok, _} = xref:start(s),
    {ok, A} = xref:add_module(s, MFile, {warnings,false}),
    {ok, [{{{A,t,1},{'$M_EXPR','$F_EXPR',2}},[7]},
          {{{A,t,1},{'$M_EXPR',bar,2}},[6]},
          {{{A,t1,1},{'$M_EXPR','$F_EXPR',1}},[11]},
          {{{A,t1,1},{A,'$F_EXPR',1}},[10]},
          {{{A,t2,3},{'$M_EXPR','$F_EXPR',-1}},[14]},
          {{{A,t2,3},{'$M_EXPR','$F_EXPR',1}},[15]},
          {{{A,t3,1},{'$M_EXPR','$F_EXPR',3}},[19]},
          {{{A,t3,1},{fun_mfa_vars,t,-1}},[18]},
          {{{A,t4,2},{'$M_EXPR','$F_EXPR',3}},[22,23]},
          {{{A,t5,2},{'$M_EXPR','$F_EXPR',0}},[27]},
          {{{A,t5,2},{'$M_EXPR',t,-1}},[26]}]} =
    xref:q(s, "(Lin) E"),

    ok = check_state(s),
    xref:stop(s),

    ok = file:delete(File),
    ok = file:delete(Beam),
    ok.

%% OTP-5195: A bug fix when using qlc:q/1,2.
qlc(Conf) when is_list(Conf) ->
    Dir = ?copydir,
    File = fname(Dir, "qlc.erl"),
    MFile = fname(Dir, "qlc"),
    Beam = fname(Dir, "qlc.beam"),
    Test = <<"-module(qlc).

              -include_lib(\"stdlib/include/qlc.hrl\").

              -export([t/0]).

              t() ->
                  dets:open_file(t, []),
                  dets:insert(t, [{1,a},{2,b},{3,c},{4,d}]),
                  MS = ets:fun2ms(fun({X,Y}) when (X > 1) or (X < 5) -> {Y}
                                  end),
                  QH1 = dets:table(t, [{traverse, {select, MS}}]),
                  QH2 = qlc:q([{Y} || {X,Y} <- dets:table(t),
                                      (X > 1) or (X < 5)]),
                  true = qlc:info(QH1) =:= qlc:info(QH2),
                  dets:close(t),
                  ok.
             ">>,

    ok = file:write_file(File, Test),
    A = qlc,
    {ok, A} = compile:file(File, [debug_info,{outdir,Dir}]),
    {ok, _} = xref:start(s),
    {ok, A} = xref:add_module(s, MFile, {warnings,false}),
    {ok, _} = xref:q(s, "(Lin) E"), % is can be loaded

    ok = check_state(s),
    xref:stop(s),

    ok = file:delete(File),
    ok = file:delete(Beam),
    ok.



%% Simple analyses
analyze(Conf) when is_list(Conf) ->
    S0 = new(),
    {{error, _, {invalid_options,[not_an_option]}}, _} =
      xref_base:analyze(S0, undefined_function_calls, [not_an_option]),
    {{error, _, {invalid_query,{q}}}, _} = xref_base:q(S0,{q}),
    {{error, _, {unknown_analysis,foo}}, _} = xref_base:analyze(S0, foo),
    {{error, _, {unknown_constant,"foo:bar/-1"}}, _} =
      xref_base:analyze(S0, {use,{foo,bar,-1}}),

    CopyDir = ?copydir,
    Dir = fname(CopyDir,"rel2"),
    X = fname(Dir, "x.erl"),
    Y = fname(Dir, "y.erl"),
    A1_1 = fname([Dir,"lib","app1-1.1"]),
    A2 = fname([Dir,"lib","app2-1.1"]),
    EB1_1 = fname(A1_1, "ebin"),
    EB2 = fname(A2, "ebin"),
    Xbeam = fname(EB2, "x.beam"),
    Ybeam = fname(EB1_1, "y.beam"),

    {ok, x} = compile:file(X, [debug_info, {outdir,EB2}]),
    {ok, y} = compile:file(Y, [debug_info, {outdir,EB1_1}]),

    {ok, rel2, S1} = xref_base:add_release(S0, Dir, [{verbose,false}]),
    S = set_up(S1),

    {ok, _} = analyze(undefined_function_calls, [{{x,xx,0},{x,undef,0}}], S),
    {ok, _} = analyze(undefined_functions, [{x,undef,0}], S),
    {ok, _} = analyze(locals_not_used, [{x,l,0},{x,l1,0}], S),
    {ok, _} = analyze(exports_not_used, [{x,xx,0},{y,t,0}], S),

    {ok, _} = analyze(deprecated_function_calls, [{{y,t,0},{x,t,0}}], S),
    {ok, _} = analyze({deprecated_function_calls,next_version}, [], S),
    {ok, _} = analyze({deprecated_function_calls,next_major_release}, [], S),
    {ok, _} = analyze({deprecated_function_calls,eventually},
                      [{{y,t,0},{x,t,0}}], S),
    {ok, _} = analyze(deprecated_functions, [{x,t,0}], S),
    {ok, _} = analyze({deprecated_functions,next_version}, [], S),
    {ok, _} = analyze({deprecated_functions,next_major_release}, [], S),
    {ok, _} = analyze({deprecated_functions,eventually}, [{x,t,0}], S),

    {ok, _} = analyze({call, {x,xx,0}}, [{x,undef,0}], S),
    {ok, _} = analyze({call, [{x,xx,0},{x,l,0}]}, [{x,l1,0},{x,undef,0}], S),
    {ok, _} = analyze({use, {x,l,0}}, [{x,l1,0}], S),
    {ok, _} = analyze({use, [{x,l,0},{x,l1,0}]}, [{x,l,0},{x,l1,0}], S),

    {ok, _} = analyze({module_call, x}, [x], S),
    {ok, _} = analyze({module_call, [x,y]}, [x], S),
    {ok, _} = analyze({module_use, x}, [x,y], S),
    {ok, _} = analyze({module_use, [x,y]}, [x,y], S),

    {ok, _} = analyze({application_call, app1}, [app2], S),
    {ok, _} = analyze({application_call, [app1,app2]}, [app2], S),
    {ok, _} = analyze({application_use, app2}, [app1,app2], S),
    {ok, _} = analyze({application_use, [app1,app2]}, [app1,app2], S),

    ok = xref_base:delete(S),
    ok = file:delete(Xbeam),
    ok = file:delete(Ybeam),
    ok.

%% Use of operators
basic(Conf) when is_list(Conf) ->
    S0 = new(),

    F1 = {m1,f1,1},
    F6 = {m1,f2,6}, % X
    F2 = {m2,f1,2},
    F3 = {m2,f2,3}, % X
    F7 = {m2,f3,7}, % X
    F4 = {m3,f1,4}, % X
    F5 = {m3,f2,5},

    UF1 = {m1,f12,17},
    UF2 = {m17,f17,177},

    E1 = {F1,F3}, % X
    E2 = {F6,F7}, % X
    E3 = {F2,F6}, % X
    E4 = {F1,F4}, % X
    E5 = {F4,F5},
    E6 = {F7,F4}, % X
    E7 = {F1,F6},

    UE1 = {F2,UF2}, % X
    UE2 = {F5,UF1}, % X

    D1 = {F1,12},
    D6 = {F6,3},
    DefAt_m1 = [D1,D6],
    X_m1 = [F6],
    % L_m1 = [F1],
    XC_m1 = [E1,E2,E4],
    LC_m1 = [E7],
    LCallAt_m1 = [{E7,12}],
    XCallAt_m1 = [{E1,13},{E2,17},{E4,7}],
    Info1 = #xref_mod{name = m1, app_name = [a1]},
    S1 = add_module(S0, Info1, DefAt_m1, X_m1, LCallAt_m1, XCallAt_m1,
                    XC_m1, LC_m1),

    D2 = {F2,7},
    D3 = {F3,9},
    D7 = {F7,19},
    DefAt_m2 = [D2,D3,D7],
    X_m2 = [F3,F7],
    % L_m2 = [F2],
    XC_m2 = [E3,E6,UE1],
    LC_m2 = [],
    LCallAt_m2 = [],
    XCallAt_m2 = [{E3,96},{E6,12},{UE1,77}],
    Info2 = #xref_mod{name = m2, app_name = [a2]},
    S2 = add_module(S1, Info2, DefAt_m2, X_m2, LCallAt_m2, XCallAt_m2,
                    XC_m2, LC_m2),

    D4 = {F4,6},
    D5 = {F5,97},
    DefAt_m3 = [D4,D5],
    X_m3 = [F4],
    % L_m3 = [F5],
    XC_m3 = [UE2],
    LC_m3 = [E5],
    LCallAt_m3 = [{E5,19}],
    XCallAt_m3 = [{UE2,22}],
    Info3 = #xref_mod{name = m3, app_name = [a3]},
    S3 = add_module(S2, Info3, DefAt_m3, X_m3, LCallAt_m3, XCallAt_m3,
                    XC_m3, LC_m3),

    Info4 = #xref_mod{name = m4, app_name = [a2]},
    S4 = add_module(S3, Info4, [], [], [], [], [], []),

    AppInfo1 = #xref_app{name = a1, rel_name = [r1]},
    S9 = add_application(S4, AppInfo1),
    AppInfo2 = #xref_app{name = a2, rel_name = [r1]},
    S10 = add_application(S9, AppInfo2),
    AppInfo3 = #xref_app{name = a3, rel_name = [r2]},
    S11 = add_application(S10, AppInfo3),

    RelInfo1 = #xref_rel{name = r1},
    S12 = add_release(S11, RelInfo1),
    RelInfo2 = #xref_rel{name = r2},
    S13 = add_release(S12, RelInfo2),

    S = set_up(S13),

    {ok, _} = eval("[m1,m2] + m:f/1", unknown_constant, S),
    {ok, _} = eval("[m1, m2, m:f/1]", type_mismatch, S),

    {ok, _} = eval("[m1, m1->m2]", type_mismatch, S),
    {ok, _} = eval("components:f/1", unknown_constant, S),
    {ok, _} = eval("'of':f/1", unknown_constant, S),
    {ok, _} = eval("of:f/1", parse_error, S),
    {ok, _} = eval("components", unknown_constant, S),
    {ok, _} = eval("[components, of, closure]", parse_error, S),
    {ok, _} = eval("[components, 'of', closure]", unknown_constant, S),

    {ok, _} = eval("[a1->a2,m1->m2]", type_mismatch, S),
    {ok, _} = eval("a1->a2,m1->m2", parse_error, S),

    {ok, _} = eval("m1->a1", type_mismatch, S),
    {ok, _} = eval("[{m1,f1,1}] : App", parse_error, S),
    {ok, _} = eval("[{m1,f1,1}] : Fun", [F1], S),
    {ok, _} = eval("range X", type_error, S),
    {ok, _} = eval("domain X", type_error, S),
    {ok, _} = eval("range M", type_error, S),
    {ok, _} = eval("domain M", type_error, S),

    % Misc.
    {ok, _} = eval("not_a_prefix_operator m1", parse_error, S),
    {ok, _} = eval(f("(Mod) ~p", [[F1,F6,F5]]), [m1,m3], S),
    {ok, _} = eval("(Lin) M - (Lin) m1",
                   [{F2,7},{F3,9},{F7,19},{F4,6},{F5,97},{UF2,0}], S),
    {ok, _} = eval(f("(Lin) M * (Lin) ~p", [[F1,F6]]),
                   [{F1,12},{F6,3}], S),

    {ok, _} = eval(f("X * ~p", [[F1, F2, F3, F4, F5]]), [F3, F4], S),
    {ok, _} = eval("X", [F6,F3,F7,F4], S),
    {ok, _} = eval("X * AM", [F6,F3,F7,F4], S),
    {ok, _} = eval("X * a2", [F3,F7], S),

    {ok, _} = eval("L * r1", [F1,F2], S),
    {ok, _} = eval("U", [UF1, UF2], S),
    {ok, _} = eval("U * AM", [UF1], S),
    {ok, _} = eval("U * UM", [UF2], S),
    {ok, _} = eval("XU * [m1, m2]", [F6,F3,F7,UF1], S),
    {ok, _} = eval("LU * [m3, m4]", [F5], S),
    {ok, _} = eval("UU", [F1,F2], S),

    {ok, _} = eval("XC | m1", [E1,E2,E4], S),
    {ok, _} = eval(f("XC | ~p", [F1]), [E1,E4], S),
    {ok, _} = eval(f("(XXL) (Lin) (XC | ~p)", [F1]),
                   [{{D1,D3},[13]},{{D1,D4},[7]}],S),
    {ok, _} = eval(f("XC | (~p + ~p)", [F1, F2]), [E1,E4,E3,UE1], S),
    {ok, _} = eval(f("(XXL) (Lin) (XC | ~p)", [F1]),
                   [{{D1,D3},[13]},{{D1,D4},[7]}], S),
    {ok, _} = eval("LC | m3", [E5], S),
    {ok, _} = eval(f("LC | ~p", [F1]), [E7], S),
    {ok, _} = eval(f("LC | (~p + ~p)", [F1, F4]), [E7, E5], S),
    {ok, _} = eval("E | m1", [E1,E2,E4,E7], S),
    {ok, _} = eval(f("E | ~p", [F1]), [E1,E7,E4], S),
    {ok, _} = eval(f("E | (~p + ~p)", [F1, F2]), [E1,E7,E4,E3,UE1], S),

    {ok, _} = eval("XC || m1", [E3,UE2], S),
    {ok, _} = eval(f("XC || ~p", [F6]), [E3], S),
    {ok, _} = eval(f("XC || (~p + ~p)", [F4, UF2]), [UE1,E4,E6], S),
    {ok, _} = eval("LC || m3", [E5], S),
    {ok, _} = eval(f("LC || ~p", [F1]), [], S),
    {ok, _} = eval(f("LC || ~p", [F6]), [E7], S),
    {ok, _} = eval(f("LC || (~p + ~p)", [F5, F6]), [E7,E5], S),
    {ok, _} = eval("E || m1", [E3,UE2,E7], S),
    {ok, _} = eval(f("E || ~p", [F6]), [E3,E7], S),
    {ok, _} = eval(f("E || (~p + ~p)", [F3,F4]), [E1,E4,E6], S),

    {ok, _} = eval(f("~p + ~p", [F1,F2]), [F1,F2], S),
    {ok, _} = eval(f("~p * ~p", [m1,[F1,F6,F2]]), [F1,F6], S),
    {ok, _} = eval(f("~p * ~p", [F1,F2]), [], S),

    %% range, domain
    {ok, _} = eval("range (E || m1)", [F6,UF1], S),
    {ok, _} = eval("domain (E || m1)", [F1,F2,F5], S),
    {ok, _} = eval(f("E | domain ~p", [[E1, {F2,F4}]]),
                   [E1,E7,E4,E3,UE1], S),

    %% components, condensation, use, call
    {ok, _} = eval("(Lin) components E", type_error, S),
    {ok, _} = eval("components (Lin) E", type_error, S),
    {ok, _} = eval("components V", type_error, S),
    {ok, _} = eval("components E + components E", type_error, S),

    {ok, _} = eval(f("range (closure E | ~p)", [[F1,F2]]),
                   [F6,F3,F7,F4,F5,UF1,UF2], S),
    {ok, _} =
    eval(f("domain (closure E || ~p)", [[UF2,F7]]), [F1,F2,F6], S),
    {ok, _} = eval("components E", [], S),
    {ok, _} = eval("components (Mod) E", [[m1,m2,m3]], S),
    {ok, _} = eval("components closure (Mod) E", [[m1,m2,m3]], S),
    {ok, _} = eval("condensation (Mod) E",
                   [{[m1,m2,m3],[m17]}], S),
    {ok, _} = eval("condensation closure (Mod) E",
                   [{[m1,m2,m3],[m17]}], S),
    {ok, _} = eval("condensation closure closure closure (Mod) E",
                   [{[m1,m2,m3],[m17]}], S),
    {ok, _} = eval("weak condensation (Mod) E",
                   [{[m1,m2,m3],[m1,m2,m3]},{[m1,m2,m3],[m17]},{[m17],[m17]}], S),
    {ok, _} = eval("strict condensation (Mod) E",
                   [{[m1,m2,m3],[m17]}], S),
    {ok, _} = eval("range condensation (Mod) E",
                   [[m17]], S),
    {ok, _} = eval("domain condensation (Mod) E",
                   [[m1,m2,m3]], S),

    %% |, ||, |||
    {ok, _} = eval("(Lin) E || V", type_error, S),
    {ok, _} = eval("E ||| (Lin) V", type_error, S),
    {ok, _} = eval("E ||| m1", [E7], S),
    {ok, _} = eval("closure E ||| m1", [E7,{F1,UF1},{F6,UF1}], S),
    {ok, _} = eval("closure E ||| [m1,m2]",
                   [{F1,UF1},{F2,F7},{F1,F7},{F6,UF1},{F2,UF1},{F7,UF1},E7,E1,E2,E3], S),
    {ok, _} = eval("AE | a1", [{a1,a1},{a1,a2},{a1,a3}], S),

    %% path ('of')
    {ok, _} = eval("(Lin) {m1,m2} of E", type_error, S),
    {ok, _} = eval("{m1,m2} of (Lin) E", type_error, S),
    [m1,m2] = eval("{m1,m2} of {m1,m2}", S),
    {ok, _} = eval("{m1,m2} of m1", type_error, S),
    {ok, _} = eval("{a3,m1} of ME", type_mismatch, S),
    [m1,m1] = eval("{m1} of ME", S),
    [m1,m1] = eval("{m1} of closure closure ME", S),
    false = eval("{m17} of ME", S),
    [m2,m1,m2] = eval("{m2} : Mod of ME", S),
    [m1,m2,m17] = eval("{m1, m17} of ME", S),
    [m1,m2,m17] = eval("m1 -> m17 of ME", S),
    {ok, _} = eval("[m1->m17,m17->m1] of ME", type_error, S),
    case eval(f("~p of E", [{F1,F7,UF1}]), S) of
        [F1,F6,F7,F4,F5,UF1] -> ok
    end,
    [a2,a1,a2] = eval("{a2} of AE", S),

    %% weak/strict
    {ok, _} = eval("weak {m1,m2}", [{m1,m1},{m1,m2},{m2,m2}], S),
    {ok, _} = eval("strict [{m1,m1},{m1,m2},{m2,m2}]", [{m1,m2}], S),
    {ok, _} = eval("range weak [{m1,m2}] : Mod", [m1,m2], S),
    {ok, _} = eval("domain strict [{m1,m1},{m1,m2},{m2,m2}]", [m1], S),

    %% #, number of
    {ok, _} = eval("# [{r1,r2}] : Rel", 1, S),
    {ok, _} = eval("# [{a3,a1}] : App", 1, S),
    {ok, _} = eval("# AE", 7, S),
    {ok, _} = eval("# ME", 8, S),
    {ok, _} = eval("# AE + # ME", 15, S),
    {ok, _} = eval("# AE * # ME", 56, S),
    {ok, _} = eval("# AE - # ME", -1, S),
    {ok, _} = eval("# E", 9, S),
    {ok, _} = eval("# V", 9, S),
    {ok, _} = eval("# (Lin) E", 9, S),
    {ok, _} = eval("# (ELin) E", 7, S),
    {ok, _} = eval("# closure E", type_error, S),
    {ok, _} = eval("# weak {m1,m2}", 3, S),
    {ok, _} = eval("#strict condensation (Mod) E", 1, S),
    {ok, _} = eval("#components closure (Mod) E", 1, S),
    {ok, _} = eval("# range strict condensation (Mod) E", 1, S),
    ok.

%% The xref:m() and xref:d() functions
md(Conf) when is_list(Conf) ->
    CopyDir = ?copydir,
    Dir = fname(CopyDir,"md"),
    X = fname(Dir, "x__x.erl"),
    Y = fname(Dir, "y__y.erl"),
    Xbeam = fname(Dir, "x__x.beam"),
    Ybeam = fname(Dir, "y__y.beam"),

    {error, _, {invalid_filename,{foo,bar}}} = xref:m({foo,bar}),
    {error, _, {invalid_filename,{foo,bar}}} = xref:d({foo,bar}),

    {ok, x__x} = compile:file(X, [debug_info, {outdir,Dir}]),
    {ok, y__y} = compile:file(Y, [debug_info, {outdir,Dir}]),

    {error, _, {no_such_module, foo_bar}} = xref:m(foo_bar),
    OldPath = code:get_path(),
    true = code:set_path([Dir | OldPath]),
    MInfo = xref:m(x__x),
    [{{x__x,t,1},{y__y,t,2}}] = info_tag(MInfo, undefined),
    [] = info_tag(MInfo, unused),
    [] = info_tag(MInfo, deprecated),
    DInfo = xref:d(Dir),
    [{{x__x,t,1},{y__y,t,2}}] = info_tag(DInfo, undefined),
    [{y__y,l,0},{y__y,l1,0}] = info_tag(DInfo, unused),
    [] = info_tag(MInfo, deprecated),

    %% Switch from 'functions' mode to 'modules' mode.
    {ok, x__x} = compile:file(X, [no_debug_info, {outdir,Dir}]),
    {ok, y__y} = compile:file(Y, [no_debug_info, {outdir,Dir}]),
    MInfoMod = xref:m(x__x),
    [{y__y,t,2}] = info_tag(MInfoMod, undefined),
    [] = info_tag(MInfo, deprecated),
    DInfoMod = xref:d(Dir),
    [{y__y,t,2}] = info_tag(DInfoMod, undefined),
    [] = info_tag(MInfo, deprecated),

    true = code:set_path(OldPath),
    ok = file:delete(Xbeam),
    ok = file:delete(Ybeam),
    ok.

%% User queries
q(Conf) when is_list(Conf) ->
    S0 = new(),
    {ok, _} = eval("'foo", parse_error, S0),
    {ok, _} = eval("TT = E, TT = V", variable_reassigned, S0),
    {ok, _} = eval("TT = E, TTT", unknown_variable, S0),
    {ok, S} = eval("TT := E", [], S0),
    {ok, S1} = eval("TT * TT * TT", [], S),
    {ok, _S2} = xref_base:forget(S1, 'TT'),
    ok.

%% Setting and getting values of query variables
variables(Conf) when is_list(Conf) ->
    Sa = new(),
    {{error, _, {invalid_options,[not_an_option]}}, _} =
    xref_base:variables(Sa, [not_an_option]),
    {error, _, {not_user_variable,foo}} = xref_base:forget(Sa, foo),
    Sa1 = set_up(Sa),
    {error, _, {not_user_variable,foo}} = xref_base:forget(Sa1, foo),
    ok = xref_base:delete(Sa1),

    S0 = new(),

    F1 = {m1,f1,1},
    F2 = {m2,f1,2},
    Lib = {lib1,f1,1}, % undefined

    E1 = {F1,F2},
    E2 = {F2,F1},
    E3 = {F1,Lib},

    D1 = {F1,12},
    DefAt_m1 = [D1],
    X_m1 = [F1],
    % L_m1 = [],
    XC_m1 = [E1,E3],
    LC_m1 = [],
    LCallAt_m1 = [],
    XCallAt_m1 = [{E1,13},{E3,17}],
    Info1 = #xref_mod{name = m1, app_name = [a1]},
    S1 = add_module(S0, Info1, DefAt_m1, X_m1, LCallAt_m1, XCallAt_m1,
                    XC_m1, LC_m1),

    D2 = {F2,7},
    DefAt_m2 = [D2],
    X_m2 = [F2],
    % L_m2 = [],
    XC_m2 = [E2],
    LC_m2 = [],
    LCallAt_m2 = [],
    XCallAt_m2 = [{E2,96}],
    Info2 = #xref_mod{name = m2, app_name = [a2]},
    S2 = add_module(S1, Info2, DefAt_m2, X_m2, LCallAt_m2, XCallAt_m2,
                    XC_m2, LC_m2),

    S = set_up(S2),

    eval("T1=E, T2=E*T1, T3 = T2*T2, T4=range T3, T5=T3|T4, T5",
         [E1,E2,E3], S),
    eval("((E*E)*(E*E)) | (range ((E*E)*(E*E)))",
         [E1,E2,E3], S),
    eval("T1=V*V,T2=T1*V,T3=V*V*V,T3",
         [F1,F2,Lib], S),
    eval("T1=V*V, T2=V*V, T1*T2",
         [F1,F2,Lib], S),

    {ok, S100} = eval("T0 := E", [E1, E2, E3], S),
    {ok, S101} = eval("T1 := E  | m1", [E1, E3], S100),
    {ok, S102} = eval("T2 := E  | m2", [E2], S101),
    {{ok, [{user, ['T0', 'T1', 'T2']}]}, _} = xref_base:variables(S102),
    {ok, S103} = xref_base:forget(S102, 'T0'),
    {{ok, [{user, ['T1', 'T2']}]}, S104} =
    xref_base:variables(S103, [user]),
    {ok, S105} = xref_base:forget(S104),
    {{ok, [{user, []}]}, S106} = xref_base:variables(S105),
    {{ok, [{predefined,_}]}, S107_0} =
    xref_base:variables(S106, [predefined]),

    {ok, S107_1} =
    eval("TT := E, TT2 := V, TT1 := TT * TT", [E1,E2,E3], S107_0),
    {{ok, [{user, ['TT', 'TT1', 'TT2']}]}, _} =
    xref_base:variables(S107_1),
    {ok, S107} = xref_base:forget(S107_1),

    CopyDir = ?copydir,
    Dir = fname(CopyDir,"lib_test"),
    Beam = fname(Dir, "lib1.beam"),

    copy_file(fname(Dir, "lib1.erl"), Beam),
    {ok, S108} =
    xref_base:set_library_path(S107, [Dir], [{verbose,false}]),
    {{error, _, _}, _} = xref_base:variables(S108, [{verbose,false}]),
    {ok, S109} = xref_base:set_library_path(S108, [], [{verbose,false}]),

    Tabs = length(ets:all()),

    {ok, S110} = eval("Eplus := closure E, TT := Eplus",
                      'closure()', S109),
    {{ok, [{user, ['Eplus','TT']}]}, S111} = xref_base:variables(S110),
    {ok, S112} = xref_base:forget(S111, ['TT','Eplus']),
    true = Tabs =:= length(ets:all()),

    {ok, NS0} = eval("Eplus := closure E", 'closure()', S112),
    {{ok, [{user, ['Eplus']}]}, NS} = xref_base:variables(NS0),
    ok = xref_base:delete(NS),
    true = Tabs =:= length(ets:all()),

    ok = file:delete(Beam),
    ok.

%% OTP-5071. Too many unused functions.
unused_locals(Conf) when is_list(Conf) ->
    Dir = ?copydir,

    File1 = fname(Dir, "a.erl"),
    MFile1 = fname(Dir, "a"),
    Beam1 = fname(Dir, "a.beam"),
    Test1 = <<"-module(a).
               -export([f/1, g/2]).

               f(X) ->
                   Y = b:f(X),
                   Z = b:g(Y),
                   start(b, h, [Z]).

               g(X, Y) ->
                   ok.

               start(M, F, A) ->
                   spawn(M, F, A).
             ">>,
    ok = file:write_file(File1, Test1),
    {ok, a} = compile:file(File1, [debug_info,{outdir,Dir}]),

    File2 = fname(Dir, "b.erl"),
    MFile2 = fname(Dir, "b"),
    Beam2 = fname(Dir, "b.beam"),
    Test2 = <<"-module(b).
               -export([f/1, g/2]).

               f(X) ->
                   io:write(\"~w\", [X]),
                   a:start(timer, sleep, [1000]).

               g(X, Y) ->
                   apply(a, g, [X, Y]).
             ">>,

    ok = file:write_file(File2, Test2),
    {ok, b} = compile:file(File2, [debug_info,{outdir,Dir}]),

    {ok, _} = xref:start(s),
    {ok, a} = xref:add_module(s, MFile1),
    {ok, b} = xref:add_module(s, MFile2),
    {ok, []} = xref:analyse(s, locals_not_used),
    ok = check_state(s),
    xref:stop(s),

    ok = file:delete(File1),
    ok = file:delete(Beam1),
    ok = file:delete(File2),
    ok = file:delete(Beam2),
    ok.


%% Format error messages
format_error(Conf) when is_list(Conf) ->
    {ok, _Pid} = start(s),
    ok = xref:set_default(s, [{verbose,false}, {warnings, false}]),

    %% Parse error messages.
    "Invalid regular expression \"add(\"" ++ _ = fstring(xref:q(s,'"add("')),
    'Invalid operator foo\n' = fatom(xref:q(s,'foo E')),
    'Invalid wildcard variable \'_Var\' (only \'_\' is allowed)\n'
      = fatom(xref:q(s,"module:function/_Var")),
    'Missing type of regular expression ".*"\n'
      = fatom(xref:q(s,'".*"')),
    'Type does not match structure of constant: \'M\' : Fun\n'
      = fatom(xref:q(s,"'M' : Fun")),
    'Type does not match structure of constant: ".*" : Fun\n'
      = fatom(xref:q(s,'".*" : Fun')),
    'Type does not match structure of constant: [m:f/1, m1:f2/3] : App\n'
      = fatom(xref:q(s,"[m:f/1,m1:f2/3] : App")),
    'Parse error on line 1: syntax error before: \'-\'\n'
      = fatom(xref:q(s,"E + -")),
    "Parse error on line 1: unterminated atom starting with 'foo'\n"
      = flatten(xref:format_error(xref:q(s,"'foo"))),
    'Parse error at end of string: syntax error before: \n'
      = fatom(xref:q(s,"E +")),
    'Parse error on line 1: syntax error before: \'Lin\'\n'
      = fatom(xref:q(s,"Lin")),

    %% Other messages
    'Variable \'QQ\' used before set\n' = fatom(xref:q(s,"QQ")),
    'Unknown constant a\n' = fatom(xref:q(s,"{a} of E")),

    %% Testing xref_parser:t2s/1.
    'Variable assigned more than once: E := E + E\n' = fatom(xref:q(s,"E:=E + E")),
    'Variable assigned more than once: E = E + E\n' = fatom(xref:q(s,"E=E + E")),
    "Operator applied to argument(s) of different or invalid type(s): "
    "E + V * V\n" = flatten(xref:format_error(xref:q(s,"E + (V * V)"))),
    {error,xref_compiler,{type_error,"(V + V) * E"}} = xref:q(s,"(V + V) * E"),
    "Type does not match structure of constant: [m:f/3 -> g:h/17] : "
    "App\n" = flatten(xref:format_error(xref:q(s,"[{{m,f,3},{g,h,17}}] : App"))),
    'Type does not match structure of constant: [m -> f, g -> h] : Fun\n'
      = fatom(xref:q(s,"[{m,f},g->h] : Fun")),
    'Type does not match structure of constant: {m, n, o} : Fun\n' =
    fatom(xref:q(s,"{m,n,o} : Fun")),
    {error,xref_compiler,{type_error,"range (Lin) V"}} =
      xref:q(s,"range ((Lin) V)"),
    {error,xref_compiler,{type_error,"condensation range E"}} =
      xref:q(s,"condensation (range E)"),
    {error,xref_compiler,{type_error,"condensation (# E + # V)"}} =
      xref:q(s,"condensation (# E + # V)"),
    {error,xref_compiler,{type_error,"range (# E + # E)"}} =
      xref:q(s,"range (#E + #E)"),
    {error,xref_compiler,{type_error,"range (# E)"}} =
      xref:q(s,"range #E"), % Hm...
    {error,xref_compiler,{type_error,"E + # E"}} =
      xref:q(s,"E + #E + #E"), % Hm...
    {error,xref_compiler,{type_error,"V * E || V | V"}} =
      xref:q(s,"V * (E || V) | V"),
    {error,xref_compiler,{type_error,"E || (E | V)"}} =
      xref:q(s,"V * E || (E | V)"),
    {error,xref_compiler,{type_error,"E * \"m\" : Mod"}} =
      xref:q(s,'E * "m" : Mod'),
    {error,xref_compiler,{type_error,"E * (\"m\":f/_ + m:\"f\"/3)"}} =
      xref:q(s,'E * ("m":f/_ + m:"f"/3)'),

    xref:stop(s),
    ok.

%% OTP-7423. Xref scanner bug.
otp_7423(Conf) when is_list(Conf) ->
    {ok, _Pid} = start(s),
    S = "E | [compiler] : App || [{erlang,
                                   size,
                                   1}] : Fun",
    {error,xref_compiler,{unknown_constant,"compiler"}} = xref:q(s,S),
    xref:stop(s),
    ok.

%% OTP-7831. Allow anonymous Xref processes.
otp_7831(Conf) when is_list(Conf) ->
    {ok, Pid1} = xref:start([]),
    xref:stop(Pid1),
    {ok, Pid2} = xref:start([{xref_mode, modules}]),
    xref:stop(Pid2),
    ok.

%% OTP-10192. Allow filenames with character codes greater than 126.
otp_10192(Conf) when is_list(Conf) ->
    PrivDir = ?privdir,
    {ok, _Pid} = xref:start(s),
    Dir = filename:join(PrivDir, "ä"),
    ok = file:make_dir(Dir),
    {ok, []} = xref:add_directory(s, Dir),
    xref:stop(s),
    ok.

%% OTP-10192. Allow filenames with character codes greater than 126.
otp_13708(Conf) when is_list(Conf) ->
    {ok, _} = start(s),
    ok = xref:set_default(s, [{verbose, true}]),
    {ok, []} = xref:q(s,"E"),
    xref:stop(s),

    CopyDir = ?copydir,
    Dir = fname(CopyDir,"lib_test"),
    {ok, _} = start(s),
    ok = xref:set_library_path(s, [Dir], [{verbose, true}]),
    xref:stop(s).

%%%
%%% Utilities
%%%

copy_file(Src, Dest) ->
    file:copy(Src, Dest).

fname(N) ->
    filename:join(N).

fname(Dir, Basename) ->
    filename:join(Dir, Basename).

new() ->
    {ok, S} = xref_base:new(),
    S.

set_up(S) ->
    {ok, S1} = xref_base:set_up(S, [{verbose, false}]),
    S1.

eval(Query, E, S) ->
    ?format("------------------------------~n", []),
    ?format("Evaluating ~p~n", [Query]),
    {Answer, NewState} = xref_base:q(S, Query, [{verbose, false}]),
    {Reply, Expected} =
    case Answer of
        {ok, R} when is_list(E) ->
            {unsetify(R), sort(E)};
        {ok, R} ->
            {unsetify(R), E};
        {error, _Module, Reason} ->
            {element(1, Reason), E}
    end,
    if
        Reply =:= Expected ->
            ?format("As expected, got ~n~p~n", [Expected]),
            {ok, NewState};
        true ->
            ?format("Expected ~n~p~nbut got ~n~p~n", [Expected, Reply]),
            not_ok
    end.

analyze(Query, E, S) ->
    ?format("------------------------------~n", []),
    ?format("Evaluating ~p~n", [Query]),
    {{ok, L}, NewState} =
    xref_base:analyze(S, Query, [{verbose, false}]),
    case {unsetify(L), sort(E)} of
        {X,X} ->
            ?format("As was expected, got ~n~p~n", [X]),
            {ok, NewState};
        {_R,_X} ->
            ?format("Expected ~n~p~nbut got ~n~p~n", [_X, _R]),
            not_ok
    end.

unsetify(S) ->
    case is_sofs_set(S) of
        true -> to_external(S);
        false -> S
    end.

%% Note: assumes S has been set up; the new state is not returned
eval(Query, S) ->
    {{ok, Answer}, _NewState} =
    xref_base:q(S, Query, [{verbose, false}]),
    unsetify(Answer).

add_module(S, XMod, DefAt, X, LCallAt, XCallAt, XC, LC) ->
    Attr = {[], [], []},
    Depr0 = {[], [], [], []},
    DBad = [],
    Depr = {Depr0,DBad},
    Data = {DefAt, LCallAt, XCallAt, LC, XC, X, Attr, Depr},
    Unres = [],
    {ok, _Module, _Bad, State} =
    xref_base:do_add_module(S, XMod, Unres, Data),
    State.

add_application(S, XApp) ->
    xref_base:do_add_application(S, XApp).

add_release(S, XRel) ->
    xref_base:do_add_release(S, XRel).

remove_module(S, M) ->
    xref_base:do_remove_module(S, M).

info_tag(Info, Tag) ->
    {value, {_Tag, Value}} = lists:keysearch(Tag, 1, Info),
    Value.

make_ufile(FileName) ->
    ok = file:write_file(FileName, term_to_binary(foo)),
    hide_file(FileName).

make_udir(Dir) ->
    ok = file:make_dir(Dir),
    hide_file(Dir).

hide_file(FileName) ->
    {ok, FileInfo} = file:read_file_info(FileName),
    NewFileInfo = FileInfo#file_info{mode = 0},
    ok = file:write_file_info(FileName, NewFileInfo).

%% Note that S has to be set up before calling this checking function.
check_state(S) ->
    Info = xref:info(S),

    modules_mode_check(S, Info),
    case info(Info, mode) of
        modules ->
            ok;
        functions ->
            functions_mode_check(S, Info)
    end.

%% The manual mentions some facts that should always hold.
%% Here they are again.
functions_mode_check(S, Info) ->
    %% F = L + X,
    {ok, F} = xref:q(S, "F"),
    {ok, F} = xref:q(S, "L + X"),

    %% V = X + L + B + U,
    {ok, V} = xref:q(S, "V"),
    {ok, V} = xref:q(S, "X + L + B + U"),

    %% X, L, B and U are disjoint.
    {ok, []} =
    xref:q(S, "X * L + X * B + X * U + L * B + L * U + B * U"),

    %% V = UU + XU + LU,
    {ok, V} = xref:q(S, "UU + XU + LU"),

    %% E = LC + XC
    {ok, E} = xref:q(S, "E"),
    {ok, E} = xref:q(S, "LC + XC"),

    %% U subset of XU,
    {ok, []} = xref:q(S, "U - XU"),

    %% LU = range LC
    {ok, []} = xref:q(S, "(LU - range LC) + (range LC - LU)"),

    %% XU = range XC
    {ok, []} = xref:q(S, "(XU - range XC) + (range XC - XU)"),

    %% LU subset F
    {ok, []} = xref:q(S, "LU - F"),

    %% UU subset F
    {ok, []} = xref:q(S, "UU - F"),

    %% ME = (Mod) E
    {ok, ME} = xref:q(S, "ME"),
    {ok, ME} = xref:q(S, "(Mod) E"),

    %% AE = (App) E
    {ok, AE} = xref:q(S, "AE"),
    {ok, AE} = xref:q(S, "(App) E"),

    %% RE = (Rel) E
    {ok, RE} = xref:q(S, "RE"),
    {ok, RE} = xref:q(S, "(Rel) E"),

    %% (Mod) V subset of M
    {ok, []} = xref:q(S, "(Mod) V - M"),

    %% range UC subset of U
    {ok, []} = xref:q(S, "range UC - U"),

    %% Some checks on the numbers returned by the info functions.

    {Resolved, Unresolved} = info(Info, no_calls),
    AllCalls = Resolved + Unresolved,
    {ok, AllCalls} = xref:q(S, "# (XLin) E + # (LLin) E"),

    {Local, Exported} = info(Info, no_functions),
    LX = Local+Exported,
    {ok, LXs} = xref:q(S, 'Extra = _:module_info/"(0|1)" + LM,
                                  # (F - Extra)'),
    true = LX =:= LXs,

    {LocalCalls, ExternalCalls, UnresCalls} =
    info(Info, no_function_calls),
    LEU = LocalCalls + ExternalCalls + UnresCalls,
    {ok, LEU} = xref:q(S, "# LC + # XC"),

    InterFunctionCalls = info(Info, no_inter_function_calls),
    {ok, InterFunctionCalls} = xref:q(S, "# EE"),

    %% And some more checks on counters...
    check_count(S),

    %% ... and more
    {ok, []} = xref:q(S, "LM - X - U - B"),

    ok.

modules_mode_check(S, Info) ->
    %% B subset of XU,
    {ok, []} = xref:q(S, "B - XU"),

    %% M = AM + LM + UM
    {ok, M} = xref:q(S, "M"),
    {ok, M} = xref:q(S, "AM + LM + UM"),

    %% DF is a subset of X U B, etc.
    {ok, []} = xref:q(S, "DF - X - B"),
    {ok, []} = xref:q(S, "DF_3 - DF"),
    {ok, []} = xref:q(S, "DF_2 - DF_3"),
    {ok, []} = xref:q(S, "DF_1 - DF_2"),

    %% AM, LM and UM are disjoint.
    {ok, []} = xref:q(S, "AM * LM + AM * UM + LM * UM"),

    %% (App) M subset of A
    {ok, []} = xref:q(S, "(App) M - A"),

    AM = info(Info, no_analyzed_modules),
    {ok, AM} = xref:q(S, "# AM"),

    A = info(Info, no_applications),
    {ok, A} = xref:q(S, "# A"),

    NoR = info(Info, no_releases),
    {ok, NoR} = xref:q(S, "# R"),

    ok.

%% Checks the counters of some of the overall and modules info functions.
%% (Applications and releases are not checked.)
check_count(S) ->
    %%{ok, R} = xref:q(S, 'R'),
    %% {ok, A} = xref:q(S, 'A'),
    {ok, M} = xref:q(S, 'AM'),

    {ok, _} = xref:q(S,
                     "Extra := _:module_info/\"(0|1)\" + LM"),

    %% info/1:
    {ok, NoR} = xref:q(S, '# R'),
    {ok, NoA} = xref:q(S, '# A'),
    {ok, NoM} = xref:q(S, '# AM'),
    {ok, NoCalls} = xref:q(S, '# (XLin) E + # (LLin) E'),
    {ok, NoFunCalls} = xref:q(S, '# E'),
    {ok, NoXCalls} = xref:q(S, '# XC'),
    {ok, NoLCalls} = xref:q(S, '# LC'),
    {ok, NoLXCalls} = xref:q(S, '# (XC * LC)'),
    NoAllCalls = NoXCalls + NoLCalls,
    {ok, NoFun} = xref:q(S, '# (F - Extra)'),
    {ok, NoICalls} = xref:q(S, '# EE'),

    Info = xref:info(S),
    NoR = info(Info, no_releases),
    NoA = info(Info, no_applications),
    NoM = info(Info, no_analyzed_modules),
    {NoResolved, NoUC} = info(Info, no_calls),
    NoCalls = NoResolved + NoUC,
    {NoLocal, NoExternal, NoUnres} = info(Info, no_function_calls),
    NoAllCalls = NoLocal + NoExternal + NoUnres,
    NoAllCalls = NoFunCalls + NoLXCalls,
    {NoLocalFuns, NoExportedFuns} = info(Info, no_functions),
    NoFun = NoLocalFuns + NoExportedFuns,
    NoICalls = info(Info, no_inter_function_calls),

    %% per module
    info_module(M, S),

    ok.

info_module([M | Ms], S) ->
    {ok, NoCalls} = per_module("T = (E | ~p : Mod), # (XLin) T + # (LLin) T",
                               M, S),
    {ok, NoFunCalls} = per_module("# (E | ~p : Mod)", M, S),
    {ok, NoXCalls} = per_module("# (XC | ~p : Mod)", M, S),
    {ok, NoLCalls} = per_module("# (LC | ~p : Mod)", M, S),
    {ok, NoLXCalls} = per_module("# ((XC * LC) | ~p : Mod)", M, S),
    NoAllCalls = NoXCalls + NoLCalls,
    {ok, NoFun} = per_module("# (F * ~p : Mod - Extra)", M, S),
    {ok, NoICalls} = per_module("# (EE | ~p : Mod)", M, S),

    [{_M,Info}] = xref:info(S, modules, M),
    {NoResolved, NoUC} = info(Info, no_calls),
    NoCalls = NoResolved + NoUC,
    {NoLocal, NoExternal, NoUnres} = info(Info, no_function_calls),
    NoAllCalls = NoLocal + NoExternal + NoUnres,
    NoAllCalls = NoFunCalls + NoLXCalls,
    {NoLocalFuns, NoExportedFuns} = info(Info, no_functions),
    NoFun = NoLocalFuns + NoExportedFuns,
    NoICalls = info(Info, no_inter_function_calls),

    info_module(Ms, S);
info_module([], _S) ->
    ok.

per_module(Q, M, S) ->
    xref:q(S, f(Q, [M])).

info(Info, What) ->
    {value, {What, Value}} = lists:keysearch(What, 1, Info),
    Value.

f(S, A) ->
    flatten(io_lib:format(S, A)).

fatom(R) ->
    list_to_atom(fstring(R)).

fstring(R) ->
    flatten(xref:format_error(R)).

start(Server) ->
    case xref:start(Server) of
        {error, {already_started, _Pid}} ->
            xref:stop(Server),
            xref:start(Server);
        R -> R
    end.

add_erts_code_path(KernelPath) ->
    VersionDirs =
    filelib:is_dir(
      filename:join(
        [code:lib_dir(),
         lists:flatten(
           ["kernel-",
            [X ||
             {kernel,_,X} <-
             application_controller:which_applications()]])])),
    case VersionDirs of
        true ->
            case code:lib_dir(erts) of
                String when is_list(String) ->
                    [KernelPath, fname(String,"ebin")];
                _Other1 ->
                    [KernelPath]
            end;
        false ->
            % Clearcase?
            PrelPath = filename:join([code:lib_dir(),"..","erts","preloaded"]),
            case filelib:is_dir(PrelPath) of
                true ->
                    [KernelPath, fname(PrelPath,"ebin")];
                false ->
                    [KernelPath]
            end
    end.
