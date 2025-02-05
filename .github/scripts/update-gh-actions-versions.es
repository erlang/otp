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

-include_lib("kernel/include/file.hrl").

main(Args) ->

    internal_init_cmd_shell(),

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
                   OriginalBranch = cmd(Opts, "git branch --show-current"),
                   run(lists:foldl(fun parse_default/2, Opts#{ original_branch => OriginalBranch }, Arguments))
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
             "forward merge them to the local maint+master branches and push those to ~ts. "
             "Do you want to want to proceed?", [Cwd, Upstream]),

    %% Get this for dependabot update before we start switching branches and other shenanigans
    SupportedMajorVersions = string:split(cmd(Opts, ".github/scripts/get-supported-versions.sh"),"\n", all),

    %% Fetch all PRs done by dependabot
    PRs = cmd(Opts, ["gh pr -R ", Upstream, " list | grep dependabot/github_actions | awk '{print $1}'"]),

    %% If string is non-empty we have some PRs that we need to deal with
    case not string:equal(PRs,"") of
        true ->

            DependabotPRs =
                lists:foldl(
                  fun(PR, Acc) ->
                          #{ ~"baseRefName" := BaseRefName, ~"headRefName" := HeadRefName } =
                              json_cmd(Opts, ["gh pr -R ", Upstream, " view --json \"baseRefName,headRefName\" ", PR]),
                          Acc#{ PR => #{ base =>  BaseRefName, head =>  HeadRefName }}
                  end, #{}, string:split(PRs,"\n", all)),

            CheckIfCheckPassed = fun(No) ->
                case  lists:all(fun(#{ ~"name" := Name, ~"state" := State}) ->
                        string:equal(Name, "license/cla") orelse string:equal(State, "SUCCESS")
                        orelse string:equal(State, "SKIPPED")
                    end, json_cmd(Opts, ["gh pr checks ", No, " --required --json \"name,state\""])) of
                        true -> true;
                        false -> io:format("Skipping ~ts as it has checks that are not done~n",[No]),
                        false
                    end
                end,

            PassedDependabotPRs = #{ PR => V || PR := V <- DependabotPRs, CheckIfCheckPassed(PR) },

            io:format("Approving and forward merge these PRs: ~ts~n",[lists:join(", ", [PR || PR := _ <- PassedDependabotPRs])]),

            NeedsApproval = fun(No) ->
                case json_cmd(Opts, ["gh pr view --json \"reviews\" ", No]) of
                    #{ ~"reviews" := [#{ ~"state" := ~"APPROVED" }|_] } -> false;
                    _ -> true
                end
            end,

            %% Approve all dependabot PRs
            [dry(Opts, ["gh pr -R ", Upstream, " review --approve ", PR]) || PR := _ <- PassedDependabotPRs,
              NeedsApproval(PR)],

            synchronize_branch(Opts, "maint"),
            synchronize_branch(Opts, "master"),

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
                                    cmd(Opts, ["git checkout master && git merge --strategy ours maint"]),
                                    [~"master", BaseName];
                                _ ->
                                    synchronize_branch(Opts, BaseName),
                                    cmd(Opts, ["git checkout ",BaseName, " && git merge --log --no-ff ", HeadName]),
                                    cmd(Opts, ["git checkout maint && git merge --strategy ours ", BaseName]),
                                    cmd(Opts, ["git checkout master && git merge maint"]),
                                    [~"master",~"maint", BaseName]
                            end
                    end, maps:to_list(PassedDependabotPRs))),

            continue(Opts, "Push ~ts to ~ts?", [lists:join(" ", UpdatedBranches), Upstream]),

            %% Push maint+master if changed
            dry(Opts, ["git push ", Upstream, " --atomic ", lists:join(" ", UpdatedBranches)]);

       false ->
            ok
    end,

    continue(Opts, "Check if dependabot.yml is uptodate?"),

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

    cmd(Opts, ["git checkout ", maps:get(original_branch, Opts)]),

    ok.

synchronize_branch(Opts, Branch) ->
    cmd(Opts, ["git fetch ", maps:get(upstream, Opts), " ", Branch, " && "
               "git checkout -B ", Branch, " FETCH_HEAD"]).

generate_dependabot_config(Versions) ->
    ["""
     ##
     ## %CopyrightBegin%
     ##
     ## Copyright Ericsson AB 2024. All Rights Reserved.
     ##
     ## Licensed under the Apache License, Version 2.0 (the "License");
     ## you may not use this file except in compliance with the License.
     ## You may obtain a copy of the License at
     ##
     ##     http://www.apache.org/licenses/LICENSE-2.0
     ##
     ## Unless required by applicable law or agreed to in writing, software
     ## distributed under the License is distributed on an "AS IS" BASIS,
     ## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
     ## See the License for the specific language governing permissions and
     ## limitations under the License.
     ##
     ## %CopyrightEnd%
     version: 2
     updates:
     """,
     [io_lib:format(
~`
  - package-ecosystem: "github-actions"
    directories: ["/","/.github/actions/*"]
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
    maybe
        false ?= maps:get(force, Opts),
        Reply = io:get_line(Prompt ++ " (Y/n) "),
        false ?= lists:member(Reply,["Y\n","y\n","\n"]),
        cmd(Opts, ["git checkout ", maps:get(original_branch, Opts)]),
        halt(0)
    end.


json_cmd(Opts, Cmd) ->
    json:decode(unicode:characters_to_binary(cmd(Opts, Cmd))).

dry(#{ dry := true } = Opts, Cmd) ->
    log(Opts, "DryRun: ~ts~n",[Cmd]),
    ok;
dry(Opts, Cmd) ->
    cmd(Opts, Cmd).

cmd(Opts = #{}, Cmd) ->
    log(Opts, "~ts...",[Cmd]),
    Res = string:trim(do_cmd(lists:flatten(unicode:characters_to_list(Cmd)))),
    log(Opts, "~ts~n", [string:replace(Res, "\n", "\\n", all)]),
    Res.

do_cmd(Cmd) ->
    do_cmd(Cmd, #{}).
do_cmd(Cmd, Opts) ->
    MaxSize = get_option(max_size, Opts, infinity),
    {SpawnCmd, SpawnOpts, SpawnInput, Eot} = mk_cmd(os:type(), validate(Cmd)),
    Port = try open_port({spawn, SpawnCmd}, [exit_status, binary, stderr_to_stdout,
                                             stream, in, hide | SpawnOpts])
           catch error:Reason ->
                   throw({open_port, Reason})
           end,
    MonRef = erlang:monitor(port, Port),
    true = port_command(Port, SpawnInput),
    {Bytes, ExitCode} = get_data(Port, MonRef, Eot, [], 0, MaxSize),
    demonitor(MonRef, [flush]),
    String = unicode:characters_to_list(Bytes),
    ReturnValue =
        if  %% Convert to unicode list if possible otherwise return bytes
            is_list(String) -> String;
            true -> binary_to_list(Bytes)
        end,
    ExitCode =/= 0 andalso error({command_failed, ReturnValue, ExitCode}),
    ReturnValue.

get_option(Opt, Options, Default) ->
    case Options of
        #{Opt := Value} -> Value;
        #{} -> Default;
        _ -> throw(badopt)
    end.

-define(KERNEL_OS_CMD_SHELL_KEY, kernel_os_cmd_shell).

mk_cmd({win32,_}, Cmd) ->
    Shell = persistent_term:get(?KERNEL_OS_CMD_SHELL_KEY),
    Command = lists:concat([Shell, " /c", Cmd]),
    {Command, [], [], <<>>};
mk_cmd(_,Cmd) ->
    %% Have to send command in like this in order to make sh commands like
    %% cd and ulimit available.
    Shell = persistent_term:get(?KERNEL_OS_CMD_SHELL_KEY),
    {Shell ++ " -s unix:cmd", [out],
     %% We insert a new line after the command, in case the command
     %% contains a comment character.
     %%
     %% The </dev/null closes stdin, which means that programs
     %% that use a closed stdin as an termination indicator works.
     %% An example of such a program is 'more'.
     %%
     %% The "echo ^D" is used to indicate that the program has executed
     %% and we should return any output we have gotten. We cannot use
     %% termination of the child or closing of stdin/stdout as then
     %% starting background jobs from os:cmd will block os:cmd.
     %%
     %% I tried changing this to be "better", but got bombarded with
     %% backwards incompatibility bug reports, so leave this as it is.
     ["(", unicode:characters_to_binary(Cmd), "\n) </dev/null; echo \"\^D$?\^D\"\n"],
     <<$\^D>>}.

internal_init_cmd_shell() ->
    Shell =
        case application:get_env(kernel, os_cmd_shell) of
            undefined ->
                internal_init_cmd_shell(os:type());
            {ok, Val} ->
                Val
        end,
    persistent_term:put(?KERNEL_OS_CMD_SHELL_KEY, Shell).
internal_init_cmd_shell({win32,Wtype}) ->
    case {os:getenv("COMSPEC"),Wtype} of
        {false,windows} -> "command.com";
        {false,_} -> "cmd";
        {Cspec,_} -> Cspec
    end;
internal_init_cmd_shell(_) ->
    %% We use an absolute path here because we do not want the path to be
    %% searched in case a stale NFS handle is somewhere in the path before
    %% the sh command.
    %%
    %% Check if the default shell is located in /bin/sh as expected usually
    %% or in /system/bin/sh as implemented on Android. The raw option is
    %% used to bypass the file server.
    case file:read_file_info("/bin/sh",[raw]) of
        {ok,#file_info{type=regular}} ->
            "/bin/sh";
        _ ->
            case file:read_file_info("/system/bin/sh",[raw]) of
                {ok,#file_info{type=regular}} ->
                    "/system/bin/sh";
                _ ->
                    "/bin/sh"
            end
    end.

validate(Term) ->
    try validate1(Term)
    catch error:_ -> throw(badarg)
    end.

validate1(Atom) when is_atom(Atom) ->
    validate1(atom_to_list(Atom));
validate1(List) when is_list(List) ->
    case validate2(List) of
        false ->
            List;
        true ->
            %% Had zeros at end; remove them...
            string:trim(List, trailing, [0])
    end.

validate2([0|Rest]) ->
    validate3(Rest);
validate2([C|Rest]) when is_integer(C), C > 0 ->
    validate2(Rest);
validate2([List|Rest]) when is_list(List) ->
    validate2(List) or validate2(Rest);
validate2([]) ->
    false.

%% Ensure that the rest is zero only...
validate3([]) ->
    true;
validate3([0|Rest]) ->
    validate3(Rest);
validate3([List|Rest]) when is_list(List) ->
    validate3(List),
    validate3(Rest).

get_data(Port, MonRef, Eot, Sofar, Size, Max) ->
    receive
	{Port, {data, Bytes}} ->
            case eot(Bytes, Eot, Size, Max) of
                more ->
                    get_data(Port, MonRef, Eot, [Sofar, Bytes],
                             Size + byte_size(Bytes), Max);
                {Last, ExitCode} ->
                    catch port_close(Port),
                    flush_until_down(Port, MonRef),
                    {iolist_to_binary([Sofar, Last]), ExitCode}
            end;
        {'DOWN', MonRef, _, _, _} ->
	    flush_exit(Port),
	    {iolist_to_binary(Sofar), 128 + 9} %% Sigkill
    end.

eot(Bs, <<>>, Size, Max) when Size + byte_size(Bs) < Max ->
    more;
eot(Bs, <<>>, Size, Max) ->
    binary:part(Bs, {0, Max - Size});
eot(Bs, Eot, Size, Max) ->
    case binary:match(Bs, Eot) of
        {Pos, _} when Size + Pos < Max ->
            Last = binary:part(Bs,{0, Pos}),
            ExitBs = binary:part(Bs, {Pos+1, byte_size(Bs)-(Pos+1)}),
            case binary:match(ExitBs, Eot) of
                {ExitPos, _ } -> {Last, binary_to_integer(binary:part(ExitBs, {0, ExitPos}))};
                _ -> {Last, 0}
            end;
        _ ->
            eot(Bs, <<>>, Size, Max)
    end.

%% When port_close returns we know that all the
%% messages sent have been sent and that the
%% DOWN message is after them all.
flush_until_down(Port, MonRef) ->
    receive
        {Port, {data, _Bytes}} ->
            flush_until_down(Port, MonRef);
        {'DOWN', MonRef, _, _, _} ->
            flush_exit(Port)
    end.

%% The exit signal is always delivered before
%% the down signal, so we can be sure that if there
%% was an exit message sent, it will be in the
%% mailbox now.
flush_exit(Port) ->
    receive
        {'EXIT',  Port,  _} ->
            ok
    after 0 ->
            ok
    end.

log(#{ verbose := true }, Fmt, Args) ->
    io:format(standard_error, Fmt, Args);
log(_, _, _) ->
    ok.

fail(String) ->
    io:format(standard_error, "~ts~n", [String]),
    halt(1).