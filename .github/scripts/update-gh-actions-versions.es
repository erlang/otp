#!/usr/bin/env escript

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

    cmd(Opts, ["git checkout -f maint && git fetch ", maps:get(upstream, Opts), " maint && git reset --hard FETCH_HEAD"]),
    cmd(Opts, ["git checkout -f master && git fetch ", maps:get(upstream, Opts), " master && git reset --hard FETCH_HEAD"]),

    %% Fetch all PRs done by dependabot
    DependabotPRs =
        lists:foldl(
          fun(PR, Acc) ->
                  #{ ~"baseRefName" := BaseRefName, ~"headRefName" := HeadRefName } =
                      json:decode(unicode:characters_to_binary(cmd(Opts, ["gh pr -R ", Upstream, " view --json \"baseRefName,headRefName\" ", PR]))),
                  Acc#{ PR => #{ base =>  BaseRefName, head =>  HeadRefName }}
          end, #{}, string:split(cmd(Opts, ["gh pr -R ", Upstream, " list | grep dependabot/github_actions | awk '{print $1}'"]),"\n")),

    if DependabotPRs =/= #{} ->

            io:format("Approving and forward merge these PRs: ~ts~n",[string:join([PR || PR := _ <- DependabotPRs], ", ")]),

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
                                    cmd(Opts, ["git checkout master && git merge --log --no-ff ", HeadName]),
                                    ["master"];
                                ~"maint" ->
                                    cmd(Opts, ["git checkout maint && git merge --log --no-ff ", HeadName]),
                                    cmd(Opts, ["git checkout master && git merge maint"]),
                                    ["master","maint"];
                                _ ->
                                    cmd(Opts, ["git checkout maint && git merge --strategy ours ", HeadName]),
                                    cmd(Opts, ["git checkout master && git merge maint"]),
                                    ["master","maint"]
                            end
                    end, maps:to_list(DependabotPRs))),

            continue(Opts, "Push ~ts to ~ts?", [string:join(UpdatedBranches, " "), Upstream]),

            %% Push maint+master if changed
            dry(Opts, ["git push ", Upstream, " --atomic ", string:join(UpdatedBranches, " ")]),

            %% Delete dependabot branches targeting master+maint and merge any PRs targeting maint-* branches
            maps:foreach(
              fun(PR, #{ head := HeadName, base := BaseName }) ->
                      case lists:member(BaseName, [~"master", ~"maint"]) of
                          true ->
                              continue(Opts, "Delete #~ts (~ts) on ~ts?", [HeadName, PR, Upstream]),
                              dry(Opts, ["git push ", Upstream, " :", HeadName]);
                          false ->
                              continue(Opts, "Merge #~ts to ~ts on ~ts?", [PR, BaseName, Upstream]),
                              dry(Opts, ["gh pr -R ", Upstream, " merge --delete-branch --merge ", PR ])
                      end
              end, DependabotPRs);
       true ->
            ok
    end,

    cmd(Opts, "git checkout master"),

    %% Check if dependabot.yml needs updating
    SupportedMajorVersions = string:split(cmd(Opts, ".github/scripts/get-major-versions-2.sh | head -3"),"\n", all),

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

    ok.

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
                                                % log(Opts = #{ }, Str) ->
                                                %     log(Opts, "~ts",[Str]).
log(#{ verbose := true }, Fmt, Args) ->
    io:format(standard_error, Fmt, Args);
log(_, _, _) ->
    ok.

fail(String) ->
    io:format(standard_error, "~ts~n", [String]),
    halt(1).