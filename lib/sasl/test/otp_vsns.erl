%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2021. All Rights Reserved.
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
%%

-module(otp_vsns).

-export([read_state/0, read_state/1, branch_latest/2, branch_vsns/2, app_vsn/3]).

read_state() ->
    File = "otp_versions.table",
    Dir = filename:dirname(code:which(?MODULE)),
    Alt0 = filename:join([Dir, File]),
    case file:read_file_info(Alt0) of
        {ok, _} ->
            read_state(Alt0);
        _ ->
            Alt1 = filename:join([Dir, "..", "priv", File]),
            case file:read_file_info(Alt1) of
                {ok, _} ->
                    read_state(Alt1);
                _ ->
                    error(no_otp_versions_table)
            end
    end.

read_state(OtpVsnsTabFile) ->
    {ok, OtpVsnsTabData} = file:read_file(OtpVsnsTabFile),
    lists:foldl(fun (OtpAppVsnsBin, Acc0) ->
                        [OtpVsnBin
                        | AppVsnBins] = binary:split(OtpAppVsnsBin,
                                                     [<<" ">>,
                                                      <<":">>,
                                                      <<"#">>],
                                                     [global, trim_all]),
                        OtpVsn = mk_vsn(OtpVsnBin),
                        AppList = lists:map(fun (AppVsnBin) ->
                                                    AppVsn = mk_vsn(AppVsnBin),
                                                    {App, _} = AppVsn,
                                                    {App, AppVsn}
                                            end,
                                            AppVsnBins),
                        AppMap = maps:from_list(AppList),
                        Acc1 = maps:put(OtpVsn, AppMap, Acc0),
                        Acc2 = update_branch(OtpVsn, Acc1),
                        update_branch_versions(OtpVsn, Acc2)
                end,
                #{},
                binary:split(OtpVsnsTabData, <<"\n">>, [global, trim_all])).

branch_latest(State, "maint-" ++ RelStr) ->
    mk_vsn_str(maps:get({maint, list_to_integer(RelStr)}, State)).

branch_vsns(State, "maint-" ++ RelStr) ->
    Rel = list_to_integer(RelStr),
    NrmlVsns = lists:map(fun (V) -> mk_vsn_str(V) end,
                         maps:get({normal_maint_vsns, Rel}, State)),
    case maps:get({maint, Rel}, State) of
        {'OTP', [_, _]} ->
            NrmlVsns;
        {'OTP', [_, _, _]} ->
            NrmlVsns;
        {'OTP', [_, _, _, _]} = Vsn ->
            add_branched_vsns(1, Vsn, NrmlVsns)
    end.

add_branched_vsns(MinorPatch,
                  {'OTP', [_Major, _Minor, _Patch, MinorPatch]} = Vsn,
                  VsnStrs) ->
    [mk_vsn_str(Vsn) | VsnStrs];
add_branched_vsns(N,
                  {'OTP', [Major, Minor, Patch, _MinorPatch]} = Vsn,
                  VsnStrs) ->
    VsnStr = mk_vsn_str({'OTP', [Major, Minor, Patch, N]}),
    add_branched_vsns(N+1, Vsn, [VsnStr | VsnStrs]).

app_vsn(State, "OTP-"++OtpVsn, App) ->
    AppMap = maps:get({'OTP', vsnstr2vsnlist(OtpVsn)}, State),
    mk_vsn_str(maps:get(list_to_atom(App), AppMap)).

mk_vsn_str({App, Vsn}) ->
    VsnStr = lists:flatten(lists:join($., lists:map(fun (V) ->
                                                            integer_to_list(V)
                                                    end, Vsn))),
    atom_to_list(App) ++ "-" ++ VsnStr.

mk_vsn(AppVsnBin) ->
    [AppBin, VsnBin] = binary:split(AppVsnBin, <<"-">>),
    {binary_to_atom(AppBin), vsnstr2vsnlist(VsnBin)}.

vsnstr2vsnlist(VsnStr) ->
    lists:map(fun (SV) when is_list(SV) ->
                      list_to_integer(SV);
                  (BV) when is_binary(BV) ->
                      binary_to_integer(BV)
              end, string:lexemes(VsnStr, ".")).

update_branch({'OTP', [Major, Minor | _] = Vsn} = OtpVsn,
              VsnMap) when length(Vsn) =< 4 ->
    Branch = {maint, Major},
    case maps:get(Branch, VsnMap, undefined) of
        undefined ->
            maps:put(Branch, OtpVsn, VsnMap);
        {'OTP', [Major, OldMinor | _] = OldVsn} ->
            if OldMinor < Minor ->
                    maps:put(Branch, OtpVsn, VsnMap);
               OldMinor > Minor ->
                    VsnMap;
               true ->
                    {Patch, MinorPatch} = get_patch_vsns(Vsn),
                    {OldPatch, OldMinorPatch} = get_patch_vsns(OldVsn),
                    if OldPatch < Patch ->
                            maps:put(Branch, OtpVsn, VsnMap);
                       OldPatch > Patch ->
                            VsnMap;
                       OldMinorPatch < MinorPatch ->
                            maps:put(Branch, OtpVsn, VsnMap);
                       true ->
                            VsnMap
                    end
            end
    end;
update_branch({'OTP', [_Major, _Minor | _]}, VsnMap) ->
    VsnMap.

update_branch_versions({'OTP', [Rel | _] = Vsn} = OtpVsn,
                       VsnMap) when length(Vsn) =< 3 ->
    %% Save normal versions. Branched versions are added on request
    %% since we do not know the right branch until all versions has
    %% been parsed.
    BranchVsnsKey = {normal_maint_vsns, Rel},
    OtherOtpVsns = case maps:get(BranchVsnsKey, VsnMap, undefined) of
                       undefined -> [];
                       Vsns -> Vsns
                   end,
    maps:put(BranchVsnsKey, [OtpVsn|OtherOtpVsns], VsnMap);
update_branch_versions({'OTP', _}, VsnMap) ->
    VsnMap.

get_patch_vsns([_Major, _Minor]) ->
    {0, 0};
get_patch_vsns([_Major, _Minor, Patch]) ->
    {Patch, 0};
get_patch_vsns([_, _, Patch, MinorPatch | _]) ->
    {Patch, MinorPatch}.
