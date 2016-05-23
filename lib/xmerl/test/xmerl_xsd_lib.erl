%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
%%% Purpose : Sub routines for test suite for the xmerl application,
%%% xmerl_xsd module.
%%%-------------------------------------------------------------------
%%% @private
%%% File    : xmerl_xsd_lib.erl
%%% Author  : Bertil Karlsson <bertil@finrod>
%%% Description : 
%%%
%%% Created : 28 Apr 2006 by Bertil Karlsson <bertil@finrod>
%%%-------------------------------------------------------------------
-module(xmerl_xsd_lib).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("xmerl.hrl").
-include("xmerl_xsd.hrl").
-include_lib("kernel/include/file.hrl").


compare_test_results(Config, ST, IT) ->
    ResST=compare_schema_test_results(ST),
    ResIT=compare_instance_test_results(IT),
    io:format("compare_test_results:~n  ST = ~p~n  IT = ~p~n  ResST = ~p~n  ResIT = ~p~n",[ST, IT, ResST, ResIT]),
    case process_reference_results(Config, ResST, ResIT) of
	error -> error;
	Diff -> return_results(Diff, ResST, ResIT, length(ST)+length(IT))
    end.

compare_schema_test_results(ST) ->
    {[N||{N, false}<-ST], [N||{N, enoent}<-ST]}.
compare_instance_test_results(IT) ->
    {[N||{N, false}<-IT], [N||{N, enoent}<-IT]}.

return_results({SkippedN, Diff},{STErrs, _},{ITErrs, _}, TotN) ->
    NumErrs = length(STErrs ++ ITErrs),
    case NumErrs == TotN of
	true when TotN > 0 ->
	    exit(all_tests_cases_failed);
	_ ->
	    return_results2(Diff, TotN - NumErrs, SkippedN, TotN)
    end.

%% return_results2(Diff,{[],[]},{[],[]},TotN) ->
%%     {comment,io_lib:format("~p successful test cases.~n"++Diff,[TotN])};
%% return_results2(Diff,{STErrs,[]},{ITErrs,[]},TotN) ->
%%     {comment,io_lib:format("Total number of test cases: ~p~nThe following ~p test cases failed: ~p~n"++Diff,[TotN,length(STErrs++ITErrs),STErrs++ITErrs])};
%% return_results2(Diff,{STErrs,STOther},{ITErrs,ITOther},TotN) ->
%%     {comment,io_lib:format("Total number of test cases: ~p~nThe following ~p test cases failed: ~p~nThe following ~p test cases was malicious ~p~~n"++Diff,[TotN,length(STErrs++ITErrs),STErrs++ITErrs,length(STOther++ITOther),STOther++ITOther])}.

return_results2(_, 0, 0, 0) ->
    {comment,io_lib:format("This test case was empty.~n", [])};
return_results2({[], [], [], []}, NumSucc, SkippedN, TotN) ->
    {comment,io_lib:format("~p successful tests, ~p skipped tests of totally ~p test cases.~n",
			   [NumSucc, SkippedN, TotN])};
return_results2({NewFail, NewSuccess, NewMal, NewNotMal}, NumSucc, SkippedN, TotN) ->
    NFComm = case NewFail of
                 [] -> "";
                 _ -> io_lib:format("These ~p tests are new failures: ~p~n",
                                    [length(NewFail), NewFail])
             end,
    NSComm = case NewSuccess of
                 [] -> "";
                 _ -> io_lib:format("These ~p skipped tests are new succeeding cases: ~p~n",
                                    [length(NewSuccess), NewSuccess])
             end,
    NMComm = case NewMal of
                 [] -> "";
                 _ -> io_lib:format("These ~p tests are now malicious: ~p~n",
                                    [length(NewMal), NewMal])
             end,
    NNMComm = case NewNotMal of
                  [] -> "";
                  _ -> io_lib:format("These ~p  skipped tests were malicious, but succeeds now: ~p~n",
                                     [length(NewNotMal), NewNotMal])
              end,
    ct:comment(io_lib:format("~p successful tests, ~p skipped tests of totally ~p test cases. ~n" ++
			     NFComm ++ NSComm ++ NMComm ++ NNMComm, [NumSucc, SkippedN, TotN])),
    [] = NewFail.

%% return_results2(Diff,{STErrs,STOther},{ITErrs,ITOther},TotN) ->
%%     {comment,io_lib:format("Total number of test cases: ~p~n The following ~p test cases failed: ~p~nThe following ~p test cases was malicious ~p~~n",[TotN,length(STErrs++ITErrs),STErrs++ITErrs,length(STOther++ITOther),STOther++ITOther])}.


process_reference_results(Config, {ErrsST, MalST}, {ErrsIT, MalIT}) ->
    {RefFailed, RefMalicious} = xsd_reference_log(Config),
    io:format("A: ~p : ~p\n\n",[RefFailed, RefMalicious]),
    AllErrs = ErrsST ++ ErrsIT,
    AllMals = MalST ++ MalIT,
    %% test cases failed now but succeeded in reference results.
    NewFailures = [X||X<-AllErrs, lists:member(X, RefFailed) == false],
    %% test cases succeeded now but failed in reference results.
    NewSucceeds = [X||X<-RefFailed, lists:member(X, AllErrs) == false],
    %% test cases malicious now but succeeded in reference results.
    NewMalicious = [X||X<-AllMals, lists:member(X, RefMalicious) == false],
    %% test cases succeeded now but malicious in reference results.
    NewNotMal = [X||X<-RefMalicious, lists:member(X, AllMals) == false],
    write_in_log(Config, AllErrs, AllMals),
    % io:format("process_reference_results:~n  AllErrs = ~p~n  NewFailures = ~p~n",[AllErrs,NewFailures]),
    {length(RefFailed) + length(RefMalicious), {NewFailures, NewSucceeds, NewMalicious, NewNotMal}}.

xsd_reference_log(Config) ->
    DataDir = datadir(Config),
    Suite = proplists:get_value(suite, Config),
    SuiteReferenceLog = 
    filename:join([DataDir,lists:concat([Suite,"_failed_cases.log"])]),
    io:format("B: ~p\n\n",[SuiteReferenceLog]),
    case file:consult(SuiteReferenceLog) of
        {ok,List} when is_list(List) ->
            io:format("C: ~p\n\n",[List]),
            case lists:keysearch(proplists:get_value(testcase, Config), 1, List) of
                {value,{_, TCRefFails}} ->
                    io:format("D: ~p\n\n",[TCRefFails]),
                    TCRefFails;
                _ ->
                    io:format("D: ~no result\n\n",[]),
                    {[], []}
            end;
        _ ->
            {[], []}
    end.

write_in_log(_Config, [], []) ->
    ok;
write_in_log(Config, AllErrs, AllMals) ->
    LogFileName = proplists:get_value(xmerl_error_log, Config),
    {ok,IO}=file:open(LogFileName, [append]),
    TestCase = proplists:get_value(testcase, Config),
    io:format(IO,"{~p,{~p,~p}}.~n", [TestCase, AllErrs, AllMals]),
    file:close(IO),
    ok.

schema_test(Config,FileName,XsdBase,Validity) ->
    ModuleName = filename:basename(FileName),
    DataDir = datadir(Config),
    case xmerl_xsd:process_schema(filename:join([DataDir, FileName]),
                                  [{xsdbase,filename:join([DataDir, XsdBase])}]) of
	{error, enoent} ->
	    {{ModuleName, enoent},#xsd_state{}};
	{Ok, S} ->
	    case Validity of
		valid when Ok == ok ->
%%		    io:format("schema_test1: Validity=valid,Ok=ok,S=~p~n",[S]),
		    {{ModuleName, S#xsd_state.errors == []}, S};
		invalid when Ok == error -> %% S is in this case an error reason
		    {{ModuleName, no_internal_error(S)}, #xsd_state{}};
		notKnown ->
		    {{ModuleName, true}, #xsd_state{}};
		valid ->
		    io:format("schema_test2: Validity=valid,Ok=~p,S=~p~n", [Ok, S]),
%%		    io:format("FileName: ~p~n",[FileName]),
		    {{ModuleName, false}, #xsd_state{}};
		_ -> %% invalid Ok == ok
		    io:format("schema_test3: Validity=~p,Ok=~p,S=~p~n", [Validity, Ok, S]),
		    {{ModuleName, false}, S}
	    end
    end.
schema_test(Config, FileName, XsdBase, Validity, AccState) ->
    ModuleName = filename:basename(FileName),
    DataDir = datadir(Config),
    case xmerl_xsd:process_schema(filename:join([DataDir, FileName]),
                                  [{xsdbase,filename:join([DataDir, XsdBase])}, AccState]) of
        {error, enoent} ->
            {{ModuleName, enoent}, AccState};
        {Ok, S} ->
            case Validity of
                valid when Ok == ok ->
                    {{ModuleName, S#xsd_state.errors == []}, S};
                invalid when Ok == error ->
                    {{ModuleName, no_internal_error(S)}, AccState};
                notKnown ->
                    {{ModuleName, true}, AccState};
                valid ->
                    {{ModuleName, false}, AccState};
                _ ->
                    {{ModuleName, false}, S}
            end
    end.
instance_test(Config, FileName, XMLBase, Validity, State) ->
    ModuleName = filename:basename(FileName),
    DataDir = datadir(Config),
    case xmerl_scan:file(filename:join([DataDir, FileName]),
                         [{xmlbase,filename:join([DataDir, XMLBase])}]) of
        {error, enoent} ->
            {ModuleName, enoent};
        {E, _} ->
            {VE, S2} = xmerl_xsd:validate(E, State),
            case Validity of
                valid when is_record(VE, xmlElement) ->
                    case S2#xsd_state.errors of
                        [] -> ok;
                        _ -> io:format("test case ~p failed.~nValidity: ~p~nValidation result:~p~n", [FileName, Validity, VE])
                    end,
                    {ModuleName, S2#xsd_state.errors == []};
                invalid when VE == error ->
                    {ModuleName, no_internal_error(S2)};
                notKnown ->
                    {ModuleName, true};
                _ ->
                    io:format("test case ~p failed.~nValidity: ~p~nValidation result:~p~n", [FileName, Validity, VE]),
                    {ModuleName,false}
            end
    end.

no_internal_error(R) ->
    case lists:keymember(internal_error,1,R) of
	true ->
	    false;
	_ ->
	    true
    end.

unpack(Config, Suite) ->
    TarFile = suite_tar(Suite),
    file:set_cwd(datadir(Config)),
    ok=erl_tar:extract(TarFile, [compressed]),
    change_mode(filename:rootname(TarFile, ".tar.gz")).
    
suite_tar(sun) ->
    "suntest.tar.gz";
suite_tar(msx) ->
    "msxsdtest.tar.gz";
suite_tar(nist) ->
    "nisttest.tar.gz".

change_mode(Files) ->
    change_mode3(Files).
change_mode2(Dir)->
    {ok, CWD} = file:get_cwd(),
    {ok, FileList} = file:list_dir(Dir),
    file:set_cwd(filename:join([CWD, Dir])),
    change_mode3(FileList),
    file:set_cwd(CWD).
change_mode3([]) ->
    ok;
change_mode3([F |Fs]) ->
    case filelib:is_dir(F) of
        true ->
            chmod(F),
            change_mode2(F);
        _ ->
            chmod(F)
    end,
    change_mode3(Fs).
    
chmod(F) ->
    case file:read_file_info(F) of
        {ok, FileInfo} ->
            Mode= FileInfo#file_info.mode,
            file:write_file_info(F, FileInfo#file_info{mode=8#00777 bor Mode});
        _ ->
            ok
    end.

rmdir(Config, Suite) ->
    file:set_cwd(datadir(Config)),
    SuiteDir = filename:rootname(suite_tar(Suite), ".tar.gz"),
    ok=rm_f_(SuiteDir).

%% Dir is a directory
rm_f_(Dir) ->
    {ok, CWD} = file:get_cwd(),
    {ok, FileList} = file:list_dir(Dir),
    file:set_cwd(filename:join([CWD, Dir])),
    rm_files(FileList),
    file:set_cwd(CWD),
    ok = file:del_dir(Dir).

rm_files([])->
    ok;
rm_files([F |Fs]) ->
    case filelib:is_dir(F) of
	true ->
	    rm_f_(F);
	_ ->
	    io:format("rm_files: ~p~n", [F]),
	    ok = file:delete(F)
    end,
    rm_files(Fs).

create_error_log_file(Config, Suite) ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    DTString=lists:concat([Y, "-", M,"-", D, "_", H, ".", Min, ".", S]),
    FileName = lists:concat([Suite, "_", DTString, ".errorlog"]),
%%    {ok,_IO} = file:open(filename:join([privdir(Config),
%% 					      FileName]),[append]).

%%    {ok,_IO} = file:open(FileName,[append]).
    io:format("error log file: ~p~n", [filename:join([privdir(Config), FileName])]),
    {ok, filename:join([privdir(Config), FileName])}.

close_error_log_file(Config) ->
    case lists:keysearch(xmerl_error_log, 1, Config) of
        {value,{_, IO}} ->
            file:close(IO);
        _ ->
            ok
    end.

privdir(Config) ->
    proplists:get_value(priv_dir, Config).
datadir(Config) ->
    proplists:get_value(data_dir, Config).
