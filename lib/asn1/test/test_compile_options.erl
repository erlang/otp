%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(test_compile_options).

-include_lib("common_test/include/ct.hrl").


-export([wrong_path/1,comp/2,path/1,ticket_6143/1,noobj/1,
	 record_name_prefix/1,verbose/1]).

%% OTP-5689
wrong_path(Config) ->
    Pid=spawn(?MODULE,comp,[self(),Config]),
    receive
	_Err ->
	    ok
    after 10000 ->
	    exit(Pid,failure),
	    error
    end.

comp(Parent,Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    OutDir = proplists:get_value(priv_dir,Config),
    Err=asn1ct:compile(DataDir++"NoImport",[{i,OutDir},{i,filename:join([DataDir,"subdir"])},{outdir,OutDir}]),
    Parent!Err.

%% OTP-5701

path(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    OutDir = proplists:get_value(priv_dir,Config),
    {ok,CWD} = file:get_cwd(),
    file:set_cwd(filename:join([DataDir,subdir])),

    ok = asn1ct:compile("../MyMerge.set.asn",[{outdir,OutDir}]),

    ok=outfiles_check(OutDir),
    outfiles_remove(OutDir),

    file:set_cwd(filename:join([DataDir,subdir,subsubdir])),
    ok = asn1ct:compile('../../MyMerge.set.asn',[{i,'..'},{outdir,OutDir}]),

    ok=outfiles_check(OutDir,outfiles2()),
    file:set_cwd(CWD),
    ok.

ticket_6143(Config) -> asn1_test_lib:compile("AA1", Config, []).

noobj(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    OutDir = proplists:get_value(priv_dir,Config),
    
    code:purge('P-Record'),
    file:delete(filename:join([OutDir,'P-Record.erl'])),
    file:delete(filename:join([OutDir,'P-Record.beam'])),
    ok=asn1ct:compile(filename:join([DataDir,"P-Record"]),
			    [noobj,{outdir,OutDir}]),
    {ok,_} = file:read_file_info(filename:join([OutDir,
						      "P-Record.erl"])),
    {error,enoent} =
	file:read_file_info(filename:join([OutDir,"P-Record.beam"])),
    {ok,_} = c:c(filename:join([OutDir,'P-Record']),
		       [{i,OutDir},{outdir,OutDir}]),
    {file,_} = code:is_loaded('P-Record'),
    
    code:purge('P-Record'),
    code:delete('P-Record'),
    code:purge('p_record'),
    code:delete('p_record'),
    file:delete(filename:join([OutDir,'P-Record.erl'])),
    file:delete(filename:join([OutDir,'P-Record.beam'])),
    file:delete(filename:join([OutDir,'p_record.erl'])),
    file:delete(filename:join([OutDir,'p_record.beam'])),
    ok = asn1ct:compile(filename:join([DataDir,"p_record.set.asn"]),
			[asn1config,ber,noobj,{outdir,OutDir}]),
    {error,enoent} =
	file:read_file_info(filename:join([OutDir,"P-Record.beam"])),
    {error,enoent} =
	file:read_file_info(filename:join([OutDir,"P-Record.erl"])),
    {error,enoent} =
	file:read_file_info(filename:join([OutDir,"p_record.beam"])),
    io:format("read_file_info: p_record.erl~n",[]),
    {ok,_} =
	file:read_file_info(filename:join([OutDir,"p_record.erl"])),
    io:format("c:c: p_record.erl~n",[]),
    {ok,_} = c:c(filename:join([OutDir,'p_record']),
		       [{i,OutDir},{outdir,OutDir}]),
    io:format("code:is_loaded: p_record.erl~n",[]),
    {file,_} = code:is_loaded('p_record'),
    io:format("file:delete: p_record.erl~n",[]),
    file:delete(filename:join([OutDir,'p_record.erl'])),
    file:delete(filename:join([OutDir,'p_record.beam'])).

verbose(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    OutDir = proplists:get_value(priv_dir,Config),
    Asn1File = filename:join([DataDir,"Comment.asn"]),

    %% Test verbose compile
    test_server:capture_start(),
    ok = asn1ct:compile(Asn1File, [{i,DataDir},{outdir,OutDir},noobj,verbose]),
    test_server:capture_stop(),
    [Line0|_] = test_server:capture_get(),
    true = lists:prefix("Erlang ASN.1 compiler", Line0),

    %% Test non-verbose compile
    test_server:capture_start(),
    ok = asn1ct:compile(Asn1File, [{i,DataDir},{outdir,OutDir},noobj]),
    test_server:capture_stop(),
    [] = test_server:capture_get(),
    ok.

outfiles_check(OutDir) ->
    outfiles_check(OutDir,outfiles1()).


outfiles_check(_OutDir,[])->
    ok;
outfiles_check(OutDir,[H|T]) ->
    io:format("File: ~p~n",[filename:join([OutDir,H])]),
    {ok,_}=file:read_file_info(filename:join([OutDir,H])),
    outfiles_check(OutDir,T).

outfiles1() ->
    ["MyMerge.asn1db","MyMerge.beam",
     "MyMerge.erl","MyMerge.hrl"].
outfiles2() ->
    ["MyMerge.beam","MyMerge.asn1db","MyMerge.erl"].

outfiles_remove(OutDir) ->
    lists:foreach(fun(F)-> file:delete(filename:join([OutDir,F])) end,
		  outfiles1()).

record_name_prefix(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    OutDir = proplists:get_value(priv_dir,Config),
    ok = b_SeqIn(DataDir,OutDir),
    ok = a_SeqIn(DataDir,OutDir).

b_SeqIn(DataDir,OutDir) ->
    asn1ct:compile(filename:join([DataDir,'Seq']),
		   [{record_name_prefix,"b_"},{outdir,OutDir}]),
    io:format("FileName: ~p~nOutDir:~p~n",
	      [filename:join([DataDir,'b_SeqIn']),OutDir]),
    {ok,_} = compile:file(filename:join([DataDir,'b_SeqIn']),
			  [{i,OutDir}]),
    'b_SeqIn' = b_SeqIn:record_name(),
    ok.

a_SeqIn(DataDir,OutDir) -> 
    asn1ct:compile(filename:join([DataDir,'Seq']),
		   [{record_name_prefix,"a_"},{outdir,OutDir}]),
    {ok,_} = compile:file(filename:join([DataDir,'a_SeqIn']),
			  [{i,OutDir}]),
    'a_SeqIn' = a_SeqIn:record_name(),
    ok.
