%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

-module(test_compile_options).

-include_lib("test_server/include/test_server.hrl").


-export([wrong_path/1,comp/2,path/1,ticket_6143/1,noobj/1,
	 record_name_prefix/1,verbose/1,warnings_as_errors/1]).

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
    DataDir = ?config(data_dir,Config),
    OutDir = ?config(priv_dir,Config),
    ?line Err=asn1ct:compile(DataDir++"NoImport",[{i,OutDir},{i,filename:join([DataDir,"subdir"])},{outdir,OutDir}]),
    Parent!Err.

%% OTP-5701

path(Config) ->
    DataDir = ?config(data_dir,Config),
    OutDir = ?config(priv_dir,Config),
    {ok,CWD} = file:get_cwd(),
    ?line file:set_cwd(filename:join([DataDir,subdir])),

    ok = asn1ct:compile("../MyMerge.set.asn",[{outdir,OutDir}]),

    ?line ok=outfiles_check(OutDir),
    ?line outfiles_remove(OutDir),

    file:set_cwd(filename:join([DataDir,subdir,subsubdir])),
    ok = asn1ct:compile('../../MyMerge.set.asn',[{i,'..'},{outdir,OutDir}]),

    ?line ok=outfiles_check(OutDir,outfiles2()),
    file:set_cwd(CWD),
    ok.

ticket_6143(Config) -> asn1_test_lib:compile("AA1", Config, []).

noobj(Config) ->
    DataDir = ?config(data_dir,Config),
    OutDir = ?config(priv_dir,Config),
    
    code:purge('P-Record'),
    file:delete(filename:join([OutDir,'P-Record.erl'])),
    file:delete(filename:join([OutDir,'P-Record.beam'])),
    ?line ok=asn1ct:compile(filename:join([DataDir,"P-Record"]),
			    [noobj,{outdir,OutDir}]),
%    ?line false = code:is_loaded('P-Record'),
    ?line {ok,_} = file:read_file_info(filename:join([OutDir,
						      "P-Record.erl"])),
    ?line {error,enoent} =
	file:read_file_info(filename:join([OutDir,"P-Record.beam"])),
    ?line {ok,_} = c:c(filename:join([OutDir,'P-Record']),
		       [{i,OutDir},{outdir,OutDir}]),
    ?line {file,_} = code:is_loaded('P-Record'),
    
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
%%     ?line false = code:is_loaded('P-Record'),
%%     ?line false = code:is_loaded('p_record'),
    ?line {error,enoent} =
	file:read_file_info(filename:join([OutDir,"P-Record.beam"])),
    ?line {error,enoent} =
	file:read_file_info(filename:join([OutDir,"P-Record.erl"])),
    ?line {error,enoent} =
	file:read_file_info(filename:join([OutDir,"p_record.beam"])),
    io:format("read_file_info: p_record.erl~n",[]),
    ?line {ok,_} =
	file:read_file_info(filename:join([OutDir,"p_record.erl"])),
    io:format("c:c: p_record.erl~n",[]),
    ?line {ok,_} = c:c(filename:join([OutDir,'p_record']),
		       [{i,OutDir},{outdir,OutDir}]),
    io:format("code:is_loaded: p_record.erl~n",[]),
    ?line {file,_} = code:is_loaded('p_record'),
    io:format("file:delete: p_record.erl~n",[]),
    file:delete(filename:join([OutDir,'p_record.erl'])),
    file:delete(filename:join([OutDir,'p_record.beam'])).

verbose(Config) when is_list(Config) ->
    DataDir = ?config(data_dir,Config),
    OutDir = ?config(priv_dir,Config),
    Asn1File = filename:join([DataDir,"Comment.asn"]),

    %% Test verbose compile
    ?line test_server:capture_start(),
    ?line ok = asn1ct:compile(Asn1File, [{i,DataDir},{outdir,OutDir},noobj,verbose]),
    ?line test_server:capture_stop(),
    ?line [Line0|_] = test_server:capture_get(),
    ?line true = lists:prefix("Erlang ASN.1 compiler", Line0),

    %% Test non-verbose compile
    ?line test_server:capture_start(),
    ?line ok = asn1ct:compile(Asn1File, [{i,DataDir},{outdir,OutDir},noobj]),
    ?line test_server:capture_stop(),
    ?line [] = test_server:capture_get(),
    ok.

warnings_as_errors(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir,Config),
    Asn1File = filename:join([PrivDir,"WERROR.asn1"]),
    OutFile = filename:join([PrivDir,"WERROR.erl"]),
    Opts = [{outdir,PrivDir},noobj,verbose],

    %% Generate WERR.asn to emit warning
    %% Warning: Wrong format of type/value
    %%     false/{'Externalvaluereference',_,'WERR',noInvokeId}
    Warn = <<"WERROR DEFINITIONS IMPLICIT TAGS ::=\n"
	     "\n"
	     "BEGIN\n"
	     "\n"
	     "InvokeId ::= CHOICE\n"
	     "{\n"
	     "    present INTEGER,\n"
	     "    absent NULL\n"
	     "}\n"
	     "\n"
	     "noInvokeId InvokeId ::= absent:NULL\n"
	     "\n"
	     "NoInvokeId InvokeId ::= {noInvokeId}\n"
	     "\n"
	     "END -- end of useful definitions.\n">>,
    ?line ok = file:write_file(Asn1File, Warn),

    %% Test warnings_as_errors compile
    ?line false = filelib:is_regular(OutFile),
    ?line {error, _} = asn1ct:compile(Asn1File, [warnings_as_errors|Opts]),
    ?line false = filelib:is_regular(OutFile),

    %% Test normal compile
    ?line ok = asn1ct:compile(Asn1File, Opts),
    ?line true = filelib:is_regular(OutFile),
    ?line ok = file:delete(OutFile),
    ok.

outfiles_check(OutDir) ->
    outfiles_check(OutDir,outfiles1()).


outfiles_check(_OutDir,[])->
    ok;
outfiles_check(OutDir,[H|T]) ->
    io:format("File: ~p~n",[filename:join([OutDir,H])]),
    ?line {ok,_}=file:read_file_info(filename:join([OutDir,H])),
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
    DataDir = ?config(data_dir,Config),
    OutDir = ?config(priv_dir,Config),
    ok = b_SeqIn(DataDir,OutDir),
    ok = a_SeqIn(DataDir,OutDir).

b_SeqIn(DataDir,OutDir) ->
    asn1ct:compile(filename:join([DataDir,'Seq']),
		   [{record_name_prefix,"b_"},{outdir,OutDir}]),
    io:format("FileName: ~p~nOutDir:~p~n",
	      [filename:join([DataDir,'b_SeqIn']),OutDir]),
    ?line {ok,_} = compile:file(filename:join([DataDir,'b_SeqIn']),
			  [{i,OutDir}]),
    ?line 'b_SeqIn' = b_SeqIn:record_name(),
    ok.

a_SeqIn(DataDir,OutDir) -> 
    asn1ct:compile(filename:join([DataDir,'Seq']),
		   [{record_name_prefix,"a_"},{outdir,OutDir}]),
    ?line {ok,_} = compile:file(filename:join([DataDir,'a_SeqIn']),
			  [{i,OutDir}]),
    ?line 'a_SeqIn' = a_SeqIn:record_name(),
    ok.
