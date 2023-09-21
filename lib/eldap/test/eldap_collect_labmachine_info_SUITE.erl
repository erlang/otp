%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2022. All Rights Reserved.
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

%%

-module(eldap_collect_labmachine_info_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([save_eldap_data/3]).

-export([
         suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-define(DAYS_TO_KEEP, 5).

save_eldap_data(Host, Data, Config0) ->
    case init_data_transfer(Host, Config0) of
        Config1 when is_list(Config1) ->
            Config =
                case Data of
                    [[_|_]|_] ->
                        lists:foldl(fun save_data/2, Config1, Data);
                    _ ->
                        save_data(Data, Config1)
                end,
            end_data_transfer(Config);

        Skip -> Skip
    end.

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{timetrap,{seconds,40}}].

all() -> [].

%%--------------------------------------------------------------------
init_per_suite(Config) -> Config.

end_per_suite(Config) -> Config.

%%%================================================================
priv_dir(Config) -> proplists:get_value(priv_dir, Config).

priv_file(Config, Name) -> filename:join(priv_dir(Config), Name).


remove_drive_letter([_DriveLetter,$:|FileName]) -> FileName;
remove_drive_letter(FileName) -> FileName.


usable_file(FileName) ->
    case file:open(FileName, [append]) of
        {ok,D} ->
            ok == file:close(D);
        _ ->
            false
    end.

%%%----------------------------------------------------------------
wsl_ify(Cmnd) ->
    case os:getenv("WSLENV") of
        false -> Cmnd;
        _ -> "wsl " ++ Cmnd
    end.

%%%================================================================
save_data(NewEntry, Config) ->
    LocalFile = proplists:get_value(local_file, Config),

    YoungEntries =
        case file:consult(LocalFile) of
            {ok, Consulted} when is_list(Consulted) ->
                lists:filter(fun(E) -> is_young(E) end,
                             Consulted);
            Other ->
                ct:log("Strange result of consult:~n~p", [Other]),
                ct:fail("Consult failed")
        end,

    {ok,D} = file:open(LocalFile, [write]),
    lists:foreach(fun(E) ->
                          io:format(D, '~p.~n', [E])
                  end, lists:usort([NewEntry|YoungEntries])),
    file:close(D),
    Config.


is_young(E) ->
    try
        Days = days_ago(proplists:get_value(date, E)),
        Days >= 0 andalso Days =< ?DAYS_TO_KEEP
    catch
        _:_ -> false                     % No or illegal date property
    end.


days_ago(D={_,_,_})->
     calendar:date_to_gregorian_days(date()) - calendar:date_to_gregorian_days(D).

%%%----------------------------------------------------------------
init_data_transfer(Host, Config) ->
    case ct:get_config(collect_host_info) of
        undefined ->
            {skip, "No 'collect_host_info' path configured"};

        Root when is_list(Root) ->
            RemoteFile = filename:join([Root, "eldap_info", Host++".data"]),
            init_data_transfer_cont(Host, Config, RemoteFile)
    end.

init_data_transfer_cont(Host, Config, RemoteFile) ->
    LocalFile = priv_file(Config, Host++".eldapdata"),

    case usable_file(LocalFile) of
        false -> ct:fail(no_local_file);
        true -> ok
    end,

    TransferType =
        case {path_type(RemoteFile), os:type()} of
            {local, {unix,_}} ->
                case usable_file(RemoteFile) of
                    true -> filesystem;
                    false -> ssh
                end;
            _ ->
                ssh
        end,

    case TransferType of
        filesystem ->
            %% 'filesystem' was concluded since it was possible
            %% to open the file in append mode
            {ok,B} = file:read_file(RemoteFile),
            ok = file:write_file(LocalFile, B);
        ssh ->
            SCP = wsl_ify("scp "++RemoteFile++" "++remove_drive_letter(LocalFile)),
            ct:pal("Run command: \"~s\"", [SCP]),
            Result = os:cmd(SCP),
            ct:pal("Command result: \"~s\"",[Result])
    end,

    [{transfer_type, TransferType}, 
     {local_file,LocalFile},
     {remote_file,RemoteFile} | Config].
    
%%%----------------------------------------------------------------
end_data_transfer(Config) ->
    LocalFile = proplists:get_value(local_file,Config),
    RemoteFile = proplists:get_value(remote_file,Config),
    case proplists:get_value(transfer_type,Config) of
        filesystem ->
            {ok,B} = file:read_file(LocalFile),
            ok = file:write_file(RemoteFile, B);
        ssh ->
            SCP = wsl_ify("scp "++remove_drive_letter(LocalFile)++" "++RemoteFile),
            ct:pal("Run command: \"~s\"", [SCP]),
            Result = os:cmd(SCP),
            ct:pal("Command result: \"~s\"",[Result])
    end,
    file:delete(LocalFile).

path_type(Path) ->
    case string:lexemes(Path, ":") of
        [_] ->
            local;
        [Host | _] ->
            case string:find(Host, "/") of
                nomatch -> remote;
                _ -> local
            end
    end.
