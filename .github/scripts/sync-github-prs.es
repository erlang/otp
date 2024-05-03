#!/usr/bin/env escript
%%! -pa jsx/_build/default/lib/jsx/ebin/
%%
%% This scripts downloads the docs + test results from an otp repo
%% into the Target folder. It tries its best to not create too large
%% files so that gh will still be happy with us when this is published to
%% gh pages
-module('sync-github-prs').
-mode(compile).

main([Repo, Target]) ->

    io:format("Updating PRs in ~ts, current PRs are: ~p~n",
              [Target, filelib:wildcard(filename:join(Target,"*"))]),

    AllOpenPrs = ghapi("gh api --paginate -X GET /repos/"++Repo++"/pulls -f state=open"),
    %% Download all updates, there really should not be any to download as they
    %% are updated when a PR is updated, but we do it anyways just to be safe.
    handle_prs(Repo, Target,AllOpenPrs),

    %% Delete any PRs that have been closed
    {ok, AllPrs} = file:list_dir(Target),
    lists:foreach(
      fun(PRNo) ->
              case lists:search(
                     fun(#{ <<"number">> := No }) ->
                        No =:= list_to_integer(PRNo)
                     end, AllOpenPrs) of
                  {value, _} ->
                      ok;
                  false ->
                      cmd("rm -rf " ++ filename:join(Target,PRNo))
              end
      end, AllPrs),

    purge_prs(Target);

main([Repo, Target, PRNo]) ->
    handle_prs(Repo, Target, [ghapi("gh api /repos/"++Repo++"/pulls/"++PRNo)]).

handle_prs(Repo, Target, AllPRs) ->

    %% We fetch all runs for the main.yaml repo. This takes a while,
    %% but for some reason when we try to filter results using either
    %%   -f event=pull_request or -f branch=Ref github decides to not
    %% return all the runs.... So we do it the slow way...
    AllRuns = ghapi(["gh api --paginate -X GET /repos/"++Repo++"/actions/workflows/main.yaml/runs"]),

    [handle_pr(Repo, Target, PR, AllRuns) || PR <- AllPRs],

    %% Remove all links and files > 50MB
    cmd(["find ",Target," -type l -exec rm -f {} \\;"]),
    cmd(["find ",Target," -type f -size +50M -exec rm -f {} \\;"]),

    ok.

%% In order to get the latest gh actions run for a PR, we have to first list
%% all workflow runs for that branch, and then look for a matching sha with the
%% current top of the PR. Github does not have any API to find it any other way.
%% See https://github.community/t/retrieve-workflow-id-for-a-given-pr/199745/4
%%   for a discussion about this.
%%
handle_pr(_Repo, Target,
          #{ <<"number">> := Number,
             <<"head">> := #{ <<"ref">> := _Ref, <<"sha">> := Sha } },
          Runs) ->
    PRDir = filename:join(Target,integer_to_list(Number)),
    case lists:search(
           fun(#{ <<"head_sha">> := HeadSha, <<"status">> := Status }) ->
                   string:equal(HeadSha, Sha) andalso string:equal(Status, <<"completed">>)
           end, maps:get(<<"workflow_runs">>, Runs)) of
        {value, Run} ->
            Ident = integer_to_list(
                      erlang:phash2(
                        {maps:get(<<"id">>,Run), ?MODULE:module_info(md5)})),
            io:format("Checking for ~ts~n", [filename:join(PRDir, Ident)]),
            case file:read_file_info(filename:join(PRDir, Ident)) of
                {error, enoent} ->
                    io:format("Did not find ~ts. Files in dir are: ~p~n",
                              [filename:join(PRDir, Ident),
                               filelib:wildcard(filename:join(PRDir, "*"))]),
                    cmd("rm -rf "++PRDir),
                    ok = file:make_dir(PRDir),
                    ok = file:write_file(filename:join(PRDir,Ident), integer_to_list(Number)),

                    #{ <<"artifacts">> := Artifacts } =
                        ghapi(["gh api --paginate -X GET ",maps:get(<<"artifacts_url">>, Run)]),

                    lists:foreach(
                      fun(#{ <<"name">> := <<"test_results">>, <<"archive_download_url">> := Url }) ->
                              cmd(["gh api ", unicode:characters_to_list(Url), " > /tmp/test_results.zip"]),
                              cmd("unzip -d /tmp/test_results /tmp/test_results.zip"),
                              cmd(["tar xvzf /tmp/test_results/test_results.tar.gz "
                                   "-C ",PRDir," make_test_dir/ct_logs --strip-components=1"]),
                              cmd("rm -rf /tmp/test_results*");
                         (#{ <<"name">> := <<"otp_doc_html">>, <<"archive_download_url">> := Url }) ->
                              cmd(["gh api ", unicode:characters_to_list(Url), " > /tmp/otp_doc_html.zip"]),
                              cmd("unzip -d /tmp/otp_doc_html /tmp/otp_doc_html.zip"),
                              cmd(["tar xvzf /tmp/otp_doc_html/otp_doc_html.tar.gz -C ",PRDir]),
                              cmd(["find ",PRDir," -name '*.pdf' -exec rm -f {} \\;"]),
                              cmd("rm -rf /tmp/otp_doc_html*");
                         (_) ->
                              ok
                      end, Artifacts),
                    CTLogsIndex = filename:join([PRDir,"ct_logs","index.html"]),
                    case file:read_file_info(CTLogsIndex) of
                        {ok, _} ->
                            CTSuiteFiles = filename:join([PRDir,"ct_logs","ct_run*","*.logs","run.*","suite.log"]),
                            lists:foreach(fun purge_suite/1, filelib:wildcard(CTSuiteFiles));
                        _ ->
                             ok = filelib:ensure_dir(CTLogsIndex),
                             ok = file:write_file(CTLogsIndex, ["No test logs found for ", Sha])
                    end,
                    %% If we ever want to de-duplicate the docs, this command will create a
                    %% stable md5sum.
                    %% (cd $dir && find doc lib erts-* -type f \! -path "lib/jinterface-*" \! -name erlresolvelinks.js \! -name index.html \! -name release_notes.html \! -name users_guide.html \! -name internal_docs.html \! -name "*.eix" -exec md5sum {} \;) | sort -k 2 | awk "{print $1}" | md5sum
                    %% where $dir is the pr directory.
                    DocIndex = filename:join([PRDir,"doc","index.html"]),
                    case file:read_file_info(DocIndex) of
                        {ok, _} -> ok;
                        _ -> ok = filelib:ensure_dir(DocIndex),
                             ok = file:write_file(DocIndex, ["No documentation found for ", Sha])
                    end;
                {ok,_} ->
                    ok
            end;
        false ->
            ok
    end.

%% We truncate the logs of all testcases of any suite that did not have any failures
purge_suite(SuiteFilePath) ->
    {ok, SuiteFile} = file:read_file(SuiteFilePath),
    SuiteDir = filename:dirname(SuiteFilePath),
    Placeholder = "<html><body>github truncated successful testcase</body></html>",
    case re:run(SuiteFile,"^=failed\s*\([0-9]+\)$",[multiline,{capture,all_but_first,binary}]) of
        {match,[<<"0">>]} ->
            io:format("Purging logs from: ~ts~n",[SuiteDir]),
            ok = file:del_dir_r(filename:join(SuiteDir,"log_private")),
            lists:foreach(
              fun(File) ->
                      case filename:basename(File) of
                          "suite" ++ _ ->
                              ok;
                          "unexpected_io" ++_ ->
                              ok;
                          "cover.html" ->
                              ok;
                          _Else ->
                              file:write_file(File,Placeholder)
                      end
              end, filelib:wildcard(filename:join(SuiteDir,"*.html")));
        _FailedTestcases ->
            io:format("Purging logs from: ~ts~n",[SuiteDir]),
            lists:foreach(
              fun(File) ->
                      {ok, B} = file:read_file(File),
                      case re:run(B,"^=== Config value:",[multiline]) of
                          {match,_} ->
                              case re:run(B,"^=== successfully completed test case",[multiline]) of
                                  {match, _} ->
                                      file:write_file(File,Placeholder);
                                  nomatch ->
                                      ok
                              end;
                          nomatch ->
                              ok
                      end
              end, filelib:wildcard(filename:join(SuiteDir,"*.html")))
    end.

%% If we have more the 10 GB of PR data we need to remove some otherwise
%% github actions will not work them. So we purge the largest files until we
%% reach the 10 GB limit.
purge_prs(Target) ->
    %% Start by deleting all data from common_test test runs as they are huge.
    os:cmd("rm -rf "++Target++"*/ct_logs/ct_run*/*common_test_test*/run*/log_private/ct_run*"),
    Files = string:split(cmd("find " ++ Target ++ " -type f -a "
                             "\\! -name suite.log.html -exec du -a {} \\+"),"\n",all),
    SortedFiles =
        lists:sort(fun([A|_]=As,[B|_]=Bs) ->
                               binary_to_integer(A) >= binary_to_integer(B)
                   end, [string:split(F,"\t") || F <- Files, F =/= <<>>]),
    purge_prs(SortedFiles, Target, get_directory_size(Target)).
purge_prs(Files, Target, Size) when Size > 10_000_000_000 ->
    {H,T} = lists:split(10, Files),
    [file:write_file(File, io_lib:format("Large file (~p bytes) truncated", [Sz]))
     || [Sz, File] <- H],
    purge_prs(T, Target, get_directory_size(Target));
purge_prs(_, _, _) ->
    ok.

get_directory_size(Dir) ->
    binary_to_integer(hd(string:split(cmd("du -b --max-depth=0 " ++ Dir),"\t"))).


ghapi(CMD) ->
    decode(cmd(CMD)).

decode(Data) ->
    try jsx:decode(Data,[{return_maps, true}, return_tail]) of
        {with_tail, Json, <<>>} ->
            Json;
        {with_tail, Json, Tail} when is_map(Json) ->
            [Key] = maps:keys(maps:remove(<<"total_count">>, Json)),
            #{ Key => lists:flatmap(
                        fun(J) -> maps:get(Key, J) end,
                        [Json | decodeTail(Tail)])
                       };
        {with_tail, Json, Tail} when is_list(Json) ->
            lists:concat([Json | decodeTail(Tail)])
    catch E:R:ST ->
            io:format("Failed to decode: ~ts",[Data]),
            erlang:raise(E,R,ST)
    end.

decodeTail(Data) ->
    try jsx:decode(Data,[{return_maps, true}, return_tail]) of
        {with_tail, Json, <<>>} ->
            [Json];
        {with_tail, Json, Tail} ->
            [Json | decodeTail(Tail)]
    catch E:R:ST ->
            io:format("Failed to decode: ~ts",[Data]),
            erlang:raise(E,R,ST)
    end.

cmd(CMD) ->
    ListCmd = unicode:characters_to_list(CMD),
    io:format("cmd: ~ts~n",[ListCmd]),
    unicode:characters_to_binary(os:cmd(ListCmd)).
