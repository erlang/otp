%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose : Test suite for the IDL preprocessor
%%----------------------------------------------------------------------

-module(ic_pp_SUITE).
-include_lib("common_test/include/ct.hrl").



%% Standard options to the ic compiler, NOTE unholy use of OutDir

-define(OUT(X), filename:join([proplists:get_value(priv_dir, Config), gen, to_list(X)])).
-define(GCC, "g++").
-define(GCC_VER, "2.95.3").

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).
-export([arg_norm/1]).
-export([cascade_norm/1]).
-export([comment_norm/1]).
-export([concat_norm/1]).
-export([define_norm/1]).
-export([if_norm/1]).
-export([if_zero/1]).
-export([misc_norm/1]).
-export([improp_nest_constr_norm/1]).
-export([inc_norm/1]).
-export([line_norm/1]).
-export([nopara_norm/1]).
-export([predef_norm/1]).
-export([predef_time_norm/1]).
-export([self_ref_norm/1]).
-export([separate_norm/1]).
-export([swallow_sc_norm/1]).
-export([unintended_grp_norm/1]).
-export([cases/0, init_per_suite/1, end_per_suite/1]).


suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    cases().

groups() -> 
    [{arg, [], [arg_norm]}, {cascade, [], [cascade_norm]},
     {comment, [], [comment_norm]},
     {concat, [], [concat_norm]},
     {define, [], [define_norm]}, {inc, [], [inc_norm]},
     {improp_nest_constr, [], [improp_nest_constr_norm]},
     {misc, [], [misc_norm]}, {line, [], [line_norm]},
     {nopara, [], [nopara_norm]},
     {predef, [], [predef_norm]},
     {predef_time, [], [predef_time_norm]},
     {self_ref, [], [self_ref_norm]},
     {separate, [], [separate_norm]},
     {swallow_sc, [], [swallow_sc_norm]},
     {unintended_grp, [], [unintended_grp_norm]},
     {'if', [],[if_norm, if_zero]}].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


init_per_suite(Config) ->
    if
	is_list(Config) ->
	    case os:type() of
		{win32, _} ->
		    {skipped, "Very unplesent to run on windows"};
		_ ->
		    check_gcc(Config)
	    end;
	true ->
	    exit("Config not a list")
    end.

check_gcc(Config) ->
    case os:find_executable(?GCC) of
	false ->
	    {skipped, 
	     lists:flatten(io_lib:format("Can not run without ~s in path", 
					 [?GCC]))};
	_ ->
	    case trim(os:cmd(?GCC++" --version")) of
		?GCC_VER++[] -> 
		    Config;
		?GCC_VER++[D|_] when is_integer(D), D>=$0, D=<$9 -> 
		    fail_gcc(?GCC_VER++[D]);
		?GCC_VER++_ ->
		    Config;
		Ver ->
		    fail_gcc(Ver)
	    end
    end.

fail_gcc(Ver) ->
    {skipped, lists:flatten(io_lib:format("Need ~s v~s, not ~s", 
					  [?GCC, ?GCC_VER, Ver]))}.

trim(S) -> lists:reverse(skip_white(lists:reverse(skip_white(S)))).

skip_white([$\s|T]) -> skip_white(T);
skip_white([$\n|T]) -> skip_white(T);
skip_white([$\r|T]) -> skip_white(T);
skip_white([$\t|T]) -> skip_white(T);
skip_white(L) -> L.
    

end_per_suite(Config) ->
    Config.


cases() -> 
    [{group, arg}, {group, cascade}, {group, comment},
     {group, concat}, {group, define}, {group, misc}, {group, 'if'},
     {group, improp_nest_constr}, {group, inc},
     {group, line}, {group, nopara}, {group, predef},
     {group, predef_time}, {group, self_ref},
     {group, separate}, {group, swallow_sc},
     {group, unintended_grp}].
    


%%--------------------------------------------------------------------
%% arg
%%--------------------------------------------------------------------
%% Checks arguments for #define.
arg_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(arg_norm),
    File = filename:join(DataDir, arg),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% cascade
%%--------------------------------------------------------------------
%% Check cascade #define.
cascade_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(cascade_norm),
    File = filename:join(DataDir, cascade),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% comment
%%--------------------------------------------------------------------
%% Check comments.
comment_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(comment_norm),
    File = filename:join(DataDir, comment),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% concat
%%--------------------------------------------------------------------
%% Check concatinations, i.e ## .
concat_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(concat_norm),
    File = filename:join(DataDir, concat),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% define
%%--------------------------------------------------------------------
%% Check misceleaneous #define.
define_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(define_norm),
    File = filename:join(DataDir, define),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% if
%%--------------------------------------------------------------------
%% Check #if, #elif, and #endif.
if_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(if_norm),
    File = filename:join(DataDir, 'if'),
    
    ok = test_file(File, DataDir),
    ok.

%% Check #if 0
if_zero(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(if_zero),
    File = filename:join(DataDir, if_zero),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% inc
%%--------------------------------------------------------------------
%% Check #include.
inc_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(inc_norm),
    File = filename:join(DataDir, inc),
    
    ok = test_file(File, DataDir),
    ok.



%%--------------------------------------------------------------------
%% improp_nest_constr
%%--------------------------------------------------------------------
%% Check improperly nested constructs.
improp_nest_constr_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(improp_nest_constr_norm),
    File = filename:join(DataDir, improp_nest_constr),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% misc
%%--------------------------------------------------------------------
%% Misceleaneous checks.
misc_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(misc_norm),
    File = filename:join(DataDir, misc),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% line
%%--------------------------------------------------------------------
%% Checks #line.
line_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(line_norm),
    File = filename:join(DataDir, line),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% nopara
%%--------------------------------------------------------------------
%% Checks #define with no parameters.
nopara_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(nopara_norm),
    File = filename:join(DataDir, nopara),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% predef
%%--------------------------------------------------------------------
%% Checks predefined macros. Note: not __TIME__ and __DATE__.
predef_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(predef_norm),
    File = filename:join(DataDir, predef),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% predef_time
%%--------------------------------------------------------------------
%% Checks the predefined macros __TIME__ and __DATE__.
predef_time_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(predef_time_norm),
    File = filename:join(DataDir, predef_time),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% self_ref
%%--------------------------------------------------------------------
%% Checks self referring macros.
self_ref_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(self_ref_norm),
    File = filename:join(DataDir, self_ref),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% separate
%%--------------------------------------------------------------------
%% Checks separete expansion of macro arguments.
separate_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(separate_norm),
    File = filename:join(DataDir, separate),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% swallow_sc
%%--------------------------------------------------------------------
%% Checks swallowing an undesirable semicolon.
swallow_sc_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(swallow_sc_norm),
    File = filename:join(DataDir, swallow_sc),
    
    ok = test_file(File, DataDir),
    ok.


%%--------------------------------------------------------------------
%% unintended_grp
%%--------------------------------------------------------------------
%% Checks unintended grouping of arithmetic.
unintended_grp_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    _OutDir = ?OUT(unintended_grp_norm),
    File = filename:join(DataDir, unintended_grp),
    
    ok = test_file(File, DataDir),
    ok.


test_file(FileT, DataDir) ->
    case test_file_1(FileT, DataDir) of
	ok -> ok;
	Chars ->
	    io:put_chars(Chars),
	    {error,{FileT,DataDir}}
    end.

test_file_1(FileT, DataDir) ->
    Tok = string:tokens(FileT, "/"),
    FileName = lists:last(Tok),
    File = FileT++".idl",

    test_server:format("File  ~p~n",[File]),
    test_server:format("FileName  ~p~n",[FileName]),

    Flags = "-I"++DataDir,

    test_server:format("Flags  ~p~n",[Flags]),

    Erl = pp_erl(File, Flags),
    Gcc = pp_gcc(File, Flags),

    case Erl of
	{error,_ErlError} ->
	    test_server:format("Internal_pp Result ~n==================~n~p~n~n",[Erl]);
	{warning, _ErlWar} ->
	    test_server:format("Internal_pp Result ~n==================~n~p~n~n",[Erl]);
	_ ->
	    test_server:format("Internal_pp Result ~n==================~n~s~n~n",[Erl])
    end,
    
    case Gcc of
	{error,GccError} ->
	    Error = string:tokens(GccError, "\n"),
	    test_server:format(?GCC" Result ~n==========~n~p~n~n",
				     [Error]);
	_ ->
	    test_server:format(?GCC" Result ~n==========~n~s~n~n",[Gcc])
    end,



    case {Erl,Gcc} of
	{{warning,W}, {error,X}} ->
	    case is_ok(W,X) of
		yes ->
		    ok;
		no ->
		    io_lib:format("Internal_pp found Warning = ~p ~n"
				  ?GCC" found Error = ~p~n",[W,X])
	    end;
	   

	{{warning,W}, _} ->
	    io_lib:format(?GCC" did not find warnings while ~n"
			  "Internal_pp found the following Warning = ~p~n",[W]);

	{{error,E}, {error,X}} ->
	    case is_ok(E,X) of
		yes ->
		    ok;
		no ->
		    io_lib:format("Internal_pp found Error = ~p ~n"
				  ?GCC" found Error = ~p~n",[E,X])
	    end;

	{{error,E}, _} ->
	    case FileName of
		"if" ->
		    case if_res(E) of
			ok ->
			    ok;
			_ ->
			    io_lib:format(?GCC" did not find errors while ~n"
					  "Internal_pp found the following Error = ~p~n",[E])
		    end;
		_ ->
		    io_lib:format(?GCC" did not find errors while ~n"
				  "Internal_pp found the following Error = ~p~n",[lists:flatten(E)])
	    end;

	{_, {error,X}} ->
	    io_lib:format("Internal_pp did not find errors while ~n"
			  ?GCC" found the following Error = ~p~n",[X]);

	_ ->

	    file:write_file("/tmp/Erl.pp",list_to_binary(Erl)),
	    file:write_file("/tmp/Gcc.pp",list_to_binary(Gcc)),
	    
	    Res = os:cmd("diff -b -w /tmp/Erl.pp /tmp/Gcc.pp"),
	    test_server:format("///////////{error,E} E ~p  FileName~p~n",[Res,FileName]),
	    case {Res, FileName} of
		{[], _} ->
		    test_server:format("Diff = []   OK!!!!!!~n"),
		    ok;
		{_, "predef_time"} ->
		    Tokens = string:tokens(Res,"\n"),
		    test_server:format("///////////{error,E} Tokens~p~n",[Tokens]),
		    case Tokens of
			["3c3",_,"---",_,"5c5",_,"---",_,"9c9",_,"---",_] ->
			    ok;
			_ ->
			    io_lib:format("Diff Result = ~p~n",[Res])
		    end;
		_ ->
		    io_lib:format("Diff Result = ~p~n",[Res])
	    end
    end.





pp_erl(File, Flags) ->
    case ic_pp:run(File,Flags) of
	{ok, [$#, $ , $1 | Rest], []} ->
	    [$#, $ , $1 | Rest];
	{ok, [$#, $ , $1 | _Rest], Warning} ->
	    {warning,Warning};
	{error,Error} ->
	    {error,Error}
    end.

pp_gcc(File, Flags) ->
    Cmd = ?GCC" -x c++ -E",
    Line	= Cmd++" "++Flags++" "++File,

    case os:cmd(Line) of
	[$#, $ , $1 | Rest] ->			
	    [$#, $ , $1 | Rest];
	Res ->

	    case string:str(Res,"# 1 \"") of
		0 ->
		    {error,Res};
		X ->
		    {error, string:sub_string(Res, 1, X-1)}
	    end
    end.


is_ok([],_Gcc) ->
    yes;
is_ok([{FileName,Line,Text}|T],Gcc) ->
    Str = FileName++":"++integer_to_list(Line)++": "++Text,
    case string:str(Gcc,Str) of
	0 ->
	    io:format("~n is_ok Internal_pp missed Error = ~s~n",[Str]),
	    no;
	_X ->
	    is_ok(T,Gcc)
    end;
is_ok([Str|T],Gcc) ->
    case string:str(Gcc,Str) of
	0 ->
	    io:format("~n is_ok Internal_pp missed Error = ~s~n",[Str]),
	    no;
	_X ->
	    is_ok(T,Gcc)
    end.


to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) -> X.



if_res(E) ->
    if_res(E,1).

if_res([H|T],Nr) ->
    %% Dir = "/clearcase/otp/libraries/ic/test/ic_pp_SUITE_data/if.idl",
    case {Nr, H} of
	{1, {_Dir, 2, "only '#if 0' is implemented at present"}} ->
	    if_res(T,Nr+1);
	{2, {_Dir, 3, "only '#if 0' is implemented at present"}} ->
	    if_res(T,Nr+1);
	{3, {_Dir, 5, "`else' command is not implemented at present"}} ->
	    if_res(T,Nr+1);
	{4, {_Dir, 9, "`elif' command is not implemented at present"}} ->
	    if_res(T,Nr+1);
	{5, {_Dir, 11, "`else' command is not implemented at present"}} ->
	    ok;
	_ ->
	    error
    end;
if_res(_, _) ->
    error.



