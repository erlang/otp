#!/usr/bin/env escript

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024. All Rights Reserved.
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

main(Args) ->

    %% Check that we have gh and ratchet
    os:find_executable("gh") =:= false andalso
        fail("Need to install gh cli tool: https://cli.github.com/"),
    os:cmd("gh auth status 2>&1 | grep 'Logged in to github.com'") =:= "" andalso
        fail("You need to be logged into github.com using gh. Please run gh auth login."),
    os:getenv("GITHUB_TOKEN") =:= false andalso
        os:putenv("GITHUB_TOKEN", string:trim(os:cmd(~s`gh auth status -t 2>&1 | grep "Token:" | awk '{print $3}'`))),
    os:find_executable("git") =:= false andalso fail("Need to install git"),

    element(1, file:read_file_info(".github/scripts/update-gh-actions-versions.es")) =/= ok andalso
        fail("Should run from root of Erlang/OTP repository"),

    argparse:run(Args, opts(), #{}).

opts() ->
    OriginalOwner = string:trim(os:cmd("gh api user -q '.login'")),
    Arguments =
        [#{ name => verbose, long => "-verbose", short => $v,
            default => false, type => boolean,
            help => "Enable debug logging" },
         #{ name => force, long => "-force", short => $f,
            default => false, type => boolean,
            help => "Do not ask for permission to do anything" },
         #{ name => dry, long => "-dry",
            default => false, type => boolean,
            help => "Do a dry run" },
         #{ name => origin, long => "-origin", short => $o,
            default => "git@github.com:" ++ OriginalOwner ++ "/otp",
            help => "Set the origin github repository, e.g git@github.com:garazdawi/otp"},
         #{ name => upstream, long => "-upstream", short => $u,
            default => ~"git@github.com:erlang/otp",
            help => "Set the upstream github repository"}],
    #{ handler =>
           fun(Opts) ->
                   run(lists:foldl(fun parse_default/2, Opts, Arguments))
           end,
       arguments => Arguments
     }.

parse_default(#{ name := Name, type := {custom, Fun}}, Opts) ->
    case maps:get(Name, Opts) of
        Value when is_binary(Value) ->
            Opts#{ Name := Fun(Value) };
        _ ->
            Opts
    end;
parse_default(_, Opts) ->
    Opts.

run(Opts) ->
    {ok, Cwd} = file:get_cwd(),

    Upstream = maps:get(upstream, Opts),
    Origin = maps:get(origin, Opts),

    continue(Opts, "This command will clean the contents of ~ts, "
             "approve and merge all open dependabot PRs, "
             "forward merge them to the local maint+master branches and push those to ~ts."
             "Do you want to want to proceed?", [Cwd, Upstream]),

    %% Get this for dependabot update before we start switching branches and other chenanigans
    SupportedMajorVersions = string:split(cmd(Opts, ".github/scripts/get-major-versions.sh | head -3"),"\n", all),
    OriginalBranch = cmd(Opts, "git branch --show-current"),

    %% Fetch all PRs done by dependabot
    PRs = cmd(Opts, ["gh pr -R ", Upstream, " list | grep dependabot/github_actions | awk '{print $1}'"]),

    %% If string is non-empty we have some PRs that we need to deal with
    case not string:equal(PRs,"") of
        true ->

            DependabotPRs =
                lists:foldl(
                  fun(PR, Acc) ->
                          #{ ~"baseRefName" := BaseRefName, ~"headRefName" := HeadRefName } =
                              json:decode(unicode:characters_to_binary(cmd(Opts, ["gh pr -R ", Upstream, " view --json \"baseRefName,headRefName\" ", PR]))),
                          Acc#{ PR => #{ base =>  BaseRefName, head =>  HeadRefName }}
                  end, #{}, string:split(PRs,"\n", all)),

            synchronize_branch(Opts, "maint"),
            synchronize_branch(Opts, "master"),

            io:format("Approving and forward merge these PRs: ~ts~n",[lists:join(", ", [PR || PR := _ <- DependabotPRs])]),

            %% Approve all dependabot PRs
            [dry(Opts, ["gh pr -R ", Upstream, " review --approve ", PR]) || PR := _ <- DependabotPRs],

            %% Create all merges to maint + master
            UpdatedBranches =
                lists:usort(
                  lists:flatmap(
                    fun({PR, #{ head := HeadName, base := BaseName }}) ->
                            cmd(Opts, ["gh pr -R ", Upstream, " checkout ", PR]),
                            case BaseName of
                                ~"master" ->
                                    cmd(Opts, ["git checkout ",BaseName, " && git merge --log --no-ff ", HeadName]),
                                    [BaseName];
                                ~"maint" ->
                                    cmd(Opts, ["git checkout ",BaseName, " && git merge --log --no-ff ", HeadName]),
                                    cmd(Opts, ["git checkout master && git merge --strategy outs maint"]),
                                    ["master", BaseName];
                                _ ->
                                    synchronize_branch(Opts, BaseName),
                                    cmd(Opts, ["git checkout ",BaseName, " && git merge --log --no-ff ", HeadName]),
                                    cmd(Opts, ["git checkout maint && git merge --strategy ours ", BaseName]),
                                    cmd(Opts, ["git checkout master && git merge maint"]),
                                    ["master","maint", BaseName]
                            end
                    end, maps:to_list(DependabotPRs))),

            continue(Opts, "Push ~ts to ~ts?", [lists:join(" ", UpdatedBranches), Upstream]),

            %% Push maint+master if changed
            dry(Opts, ["git push ", Upstream, " --atomic ", lists:join(" ", UpdatedBranches)]),

            %% Delete dependabot branches targeting master+maint and merge any PRs targeting maint-* branches
            maps:foreach(
              fun(PR, #{ head := HeadName }) ->
                      continue(Opts, "Delete #~ts (~ts) on ~ts?", [HeadName, PR, Upstream]),
                      dry(Opts, ["git push ", Upstream, " :", HeadName])
              end, DependabotPRs);
       false ->
            ok
    end,

    synchronize_branch(Opts, "master"),

    NewConfig = unicode:characters_to_binary(generate_dependabot_config(SupportedMajorVersions)),
    {ok, CurrentConfig} = file:read_file(".github/dependabot.yml"),
    case string:equal(NewConfig, CurrentConfig) of
        true ->
            io:format(".github/dependabot.yml is uptodate\n");
        false ->
            continue(Opts, ".github/dependabot.yml is invalid, do you want to create a PR that updates it?"),
            cmd(Opts, ["git fetch ", Upstream, " master && git checkout -B update-dependabot-config FETCH_HEAD"]),
            file:write_file(".github/dependabot.yml", NewConfig),
            cmd(Opts, "git add -u && git commit -m 'Update dependabot config'"),
            dry(Opts, ["git push ", Origin, " +update-dependabot-config"]),
            {match, [OriginOwner]} = re:run(Origin,":([^/]+)/",[unicode, {capture, all_but_first, list}]),
            dry(Opts, ["gh pr -R ", Upstream, " create -a '@me' -H '", OriginOwner, ":update-dependabot-config' -t 'Update dependabot config' -b ''"])
    end,

    cmd(Opts, ["git checkout ", OriginalBranch]),

    ok.

synchronize_branch(Opts, Branch) ->
    cmd(Opts, ["git fetch ", maps:get(upstream, Opts), " ", Branch, " && "
               "git checkout -B ", Branch, " FETCH_HEAD"]).

generate_dependabot_config(Versions) ->
    ["version: 2\n\nupdates:\n",
     [io_lib:format(
~`
  - package-ecosystem: "github-actions"
    directory: "/"
    target-branch: "~ts"
    schedule:
      interval: "weekly"
    labels:
      - "team:VM"
    assignees:
      - "garazdawi"
      - "kikofernandez"
    open-pull-requests-limit: 10
    groups:
      github-actions:
        patterns: ['*']`, [Branch]) || Branch <- ["master","maint"] ++ ["maint-" ++ Vsn || Vsn <- Versions]]].

continue(Opts, Format, Args) ->
    continue(Opts, io_lib:format(Format, Args)).
continue(Opts, Prompt) ->
    maps:get(force, Opts) orelse
        lists:member(
          io:get_line(Prompt ++ " (Y/n) "),
          ["Y\n","y\n","\n"]) orelse halt(0).

dry(#{ dry := true } = Opts, Cmd) ->
    log(Opts, "DryRun: ~ts~n",[Cmd]),
    ok;
dry(Opts, Cmd) ->
    cmd(Opts, Cmd).
cmd(Opts = #{}, Cmd) ->
    log(Opts, "~ts...",[Cmd]),
    Res = string:trim(os:cmd(lists:flatten(unicode:characters_to_list(Cmd)))),
    log(Opts, "~ts~n", [string:replace(Res, "\n", "\\n", all)]),
    Res.

log(#{ verbose := true }, Fmt, Args) ->
    io:format(standard_error, Fmt, Args);
log(_, _, _) ->
    ok.

fail(String) ->
    io:format(standard_error, "~ts~n", [String]),
    halt(1).