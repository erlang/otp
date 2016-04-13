-module(file_name_SUITE).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

%%
%% File operations that take filenames as parameters (* not prim_file operation) (** a drive):
%% altname
%% copy (*)
%% del_dir
%% delete
%% get_cwd (**)
%% list_dir
%% make_dir
%% make_link
%% make_symlink
%% open
%% read_file
%% read_file_info
%% read_link
%% read_link_info
%% rename
%% set_cwd
%% write_file
%% write_file_info
%%
%% File operations that opens/uses separate driver port (not connected to file)
%% altname
%% del_dir
%% delete
%% get_cwd
%% list_dir
%% make_dir
%% make_link
%% make_symlink
%% read_file_info
%% read_link
%% read_link_info
%% rename
%% set_cwd
%% write_file_info
%% 
%% Operations that use ?FD_DRV in prim_file
%% open
%% read_file
%% write_file
%%
%%
%% Operations that return a filename/path
%% altname
%% get_cwd
%% list_dir
%% read_link

-export([all/0,groups/0,suite/0,
	 init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2]).
-export([normal/1,icky/1,very_icky/1,normalize/1,home_dir/1]).


init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [normal, icky, very_icky, normalize, home_dir].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%% Check that Erlang can be started with unicode named home directory.
home_dir(Config) when is_list(Config) ->
    try
	Name=[960,945,964,961,953,954],
	Priv = proplists:get_value(priv_dir, Config),
	UniMode = file:native_name_encoding() =/= latin1,
	if 
	    not UniMode ->
		throw(need_unicode_mode);
	    true ->
		ok
	end,
	NewHome=filename:join(Priv,Name),
	file:make_dir(NewHome),
	{SaveOldName,SaveOldValue} = case os:type() of
					 {win32,nt} ->
					     HomePath=re:replace(filename:nativename(NewHome),"^[a-zA-Z]:","",[{return,list},unicode]),
					     Save = os:getenv("HOMEPATH"),
					     os:putenv("HOMEPATH",HomePath),
					     {"HOMEPATH",Save};
					 {unix,_} ->
					     Save = os:getenv("HOME"),
					     os:putenv("HOME",NewHome),
					     {"HOME",Save};
					 _ ->
					     rm_rf(prim_file,NewHome),
					     throw(unsupported_os)
				     end,
	try
	    {ok,Node} = test_server:start_node(test_unicode_homedir,slave,[{args,"-setcookie "++atom_to_list(erlang:get_cookie())}]),
	    test_server:stop_node(Node),
	    ok
	after
	    case SaveOldValue of
		false ->
		    os:unsetenv(SaveOldName);
		_ ->
		    os:putenv(SaveOldName,SaveOldValue)
	    end,
	    rm_rf(prim_file,NewHome)
	end
    catch
	throw:need_unicode_mode ->
	    io:format("Sorry, can only run in unicode mode.~n"),
	    {skipped,"VM needs to be started in Unicode filename mode"};
	throw:unsupported_os ->
	    io:format("Sorry, can only run on Unix/Windows.~n"),
	    {skipped,"Runs only on Unix/Windows"}
    end.

%% Check that filename normalization works.
normalize(Config) when is_list(Config) ->
    rand:seed(exsplus, {1290,431421,830412}),
    try
	UniMode = file:native_name_encoding() =/= latin1,
	if 
	    not UniMode ->
		throw(need_unicode_mode);
	    true ->
		ok
	end,
	Pairs = [rand_comp_decomp(200) || _ <- lists:seq(1,1000)],
	case os:type() of
	    {unix,darwin} ->
		[ true = (A =:= prim_file:internal_native2name(B)) ||
		    {A,B} <- Pairs ];
	    _ ->
		ok
	end,
	[ true = (A =:= prim_file:internal_normalize_utf8(B)) ||
	    {A,B} <- Pairs ]

    catch
	throw:need_unicode_mode ->
	    io:format("Sorry, can only run in unicode mode.~n"),
	    {skipped,"VM needs to be started in Unicode filename mode"}
    end.

%% Check file operations on normal file names regardless of unicode mode.
normal(Config) when is_list(Config) ->
    {ok,Dir} = file:get_cwd(),
    try
	Priv = proplists:get_value(priv_dir, Config),
	file:set_cwd(Priv),
	ok = check_normal(prim_file),
	ok = check_normal(file),
	%% If all is good, delete dir again (avoid hanging dir on windows)
	rm_rf(file,"normal_dir"),
	ok
    after
	file:set_cwd(Dir)
    end.


%% Check file operations on normal file names regardless of unicode mode.
icky(Config) when is_list(Config) ->
    case hopeless_darwin() of
	true ->
	    {skipped,"This version of darwin does not support icky names at all."};
	false ->
	    {ok,Dir} = file:get_cwd(),
	    try
		Priv = proplists:get_value(priv_dir, Config),
		file:set_cwd(Priv),
		ok = check_icky(prim_file),
		ok = check_icky(file),
		%% If all is good, delete dir again (avoid hanging dir on windows)
		rm_rf(file,"icky_dir"),
		ok
	    after
		file:set_cwd(Dir)
	    end
    end.
%% Check file operations on normal file names regardless of unicode mode.
very_icky(Config) when is_list(Config) ->
    case hopeless_darwin() of
	true ->
	    {skipped,"This version of darwin does not support icky names at all."};
	false ->
	    {ok,Dir} = file:get_cwd(),
	    try
		Priv = proplists:get_value(priv_dir, Config),
		file:set_cwd(Priv),
		case check_very_icky(prim_file) of
		    need_unicode_mode ->
			{skipped,"VM needs to be started in Unicode filename mode"};
		    ok ->
			ok = check_very_icky(file),
			%% If all is good, delete dir again
			%% (avoid hanging dir on windows)
			rm_rf(file,"very_icky_dir"),
			ok
		end
	    after
		file:set_cwd(Dir)
	    end
    end.


check_normal(Mod) -> 
    {ok,Dir} = Mod:get_cwd(),
    try
	NormalDir = make_normal_dir(Mod, "normal_dir"),
	io:format("Normaldir = ~p\n", [NormalDir]),
	L1 = lists:sort(list(NormalDir)),
	{ok, L0} = Mod:list_dir("."),
	io:format("L0 = ~p\n", [L0]),
	L1 = lists:sort(L0),
	{ok,D2} = Mod:get_cwd(),
	true = is_list(D2),
	case Mod:altname("fil1") of
	    {error,enotsup} ->
		ok;
	    {ok,LLL} when is_list(LLL) ->
		ok
	end,
	[ true = is_list(El) || El <- L1],
	Syms = [ {S,Targ,list_to_binary(get_data(Targ, NormalDir))}
		 || {T,S,Targ} <- NormalDir, T =:= symlink ],
	[ {ok, Cont} = Mod:read_file(SymL) || {SymL,_,Cont} <- Syms ],
	[ {ok, Targ} = fixlink(Mod:read_link(SymL)) || {SymL,Targ,_} <- Syms ],

	{ok,BeginAt} = Mod:get_cwd(),
	true = is_list(BeginAt),
	TempDir = "temp_dir",
	make_normal_dir(Mod, TempDir),
	{error,enoent} = Mod:set_cwd("tmp_dir"),
	{ok, NowAt} = Mod:get_cwd(),
	true = BeginAt =/= NowAt,
	ok = Mod:set_cwd(".."),
	{ok,BeginAt} = Mod:get_cwd(),
	rm_r(Mod, TempDir),
	true = is_list(Dir),
	[ true = is_list(FN) || FN <- L0 ],
	case Mod:make_link("fil1","nisse") of
	    ok ->
		{ok, <<"fil1">>} = Mod:read_file("nisse"),
		{ok, #file_info{type = regular}} = Mod:read_link_info("nisse"),
		ok = Mod:delete("nisse"),
		{ok, <<"fil1">>} = Mod:read_file("fil1"),
		{error,enoent} = Mod:read_file("nisse"),
		{error,enoent} = Mod:read_link_info("nisse");
	    {error,enotsup} ->
		ok
	end,
	[ begin
	      {ok, FD} = Mod:open(Name,[read]),
	      {ok, Content} = Mod:read(FD,1024),
	      ok = file:close(FD)
	  end || {regular,Name,Content} <- NormalDir ],
	[ begin
	      {ok, FD} = Mod:open(Name,[read,binary]),
	      BC = list_to_binary(Content),
	      {ok, BC} = Mod:read(FD,1024),
	      ok = file:close(FD)
	  end || {regular,Name,Content} <- NormalDir ],
	Mod:rename("fil1","tmp_fil1"),
	{ok, <<"fil1">>} = Mod:read_file("tmp_fil1"),
	{error,enoent} = Mod:read_file("fil1"),
	Mod:rename("tmp_fil1","fil1"),
	{ok, <<"fil1">>} = Mod:read_file("fil1"),
	{error,enoent} = Mod:read_file("tmp_fil1"),
	{ok,FI} = Mod:read_file_info("fil1"),
	NewMode = FI#file_info.mode band (bnot 8#333),
	NewMode2 = NewMode bor 8#222,
	true = NewMode2 =/= NewMode,
	ok = Mod:write_file_info("fil1",FI#file_info{mode = NewMode}),
	{ok,#file_info{mode = NewMode}} = Mod:read_file_info("fil1"),
	ok = Mod:write_file_info("fil1",FI#file_info{mode = NewMode2}),
	{ok,#file_info{mode = NewMode2}} = Mod:read_file_info("fil1"),
	ok
    after
        case Mod:read_file_info("fil1") of
	    {ok,FII} ->
		NewModeI = FII#file_info.mode bor 8#777,
		Mod:write_file_info("fil1",FII#file_info{mode = NewModeI});
	    _ ->
		ok
	end,
	Mod:set_cwd(Dir),
        io:format("Wd now: ~s~n",[Dir])
    end.

check_icky(Mod) -> 
    {ok,Dir} = Mod:get_cwd(),
    try
	true=(length("åäö") =:= 3),
	UniMode = file:native_name_encoding() =/= latin1,
	IckyDir = make_icky_dir(Mod, "icky_dir"),
 	{ok, L0} = Mod:list_dir_all("."),
	L1 = lists:sort(L0),
	io:format("~p~n~p~n~n",[L1,lists:sort(list(IckyDir))]),
	L1 = lists:sort(convlist(list(IckyDir))),
	{ok,D2} = Mod:get_cwd(),
	true = is_list(D2),
	%% Altname only on windows, and there are no non native filenames there
	%% 	case Mod:altname("fil1") of
	%% 	    {error,enotsup} ->
	%% 		ok;
	%% 	    {ok,LLL} when is_list(LLL) ->
	%% 		ok
	%% 	end,
	[ true = ((is_list(El) or (UniMode and is_binary(El))))  || El <- L1],
	Syms = [ {S,conv(Targ),list_to_binary(get_data(Targ,IckyDir))}
		 || {T,S,Targ} <- IckyDir, T =:= symlink ],
	[ {ok, Cont} = Mod:read_file(SymL) || {SymL,_,Cont} <- Syms ],
 	[ {ok, Targ} = fixlink(Mod:read_link_all(SymL)) ||
	    {SymL,Targ,_} <- Syms ],

	{ok,BeginAt} = Mod:get_cwd(),
	true = is_list(BeginAt),
        _ = make_icky_dir(Mod, "åäö_dir"),
        {error,enoent} = Mod:set_cwd("åä_dir"),
	{ok, NowAt} = Mod:get_cwd(),
	true = is_list(NowAt),
	true = BeginAt =/= NowAt,
	ok = Mod:set_cwd(".."),
	{ok,BeginAt} = Mod:get_cwd(),
        rm_r2(Mod,"åäö_dir"),
	{OS,_} = os:type(),

	%% Check that treat_icky really converts to the same as the OS
	case UniMode of
	    true ->
		ok = Mod:make_dir("åäö_dir"),
		ok = Mod:set_cwd("åäö_dir"),
		ok = Mod:write_file(<<"ååå">>,<<"hello">>),
		Treated = treat_icky(<<"ååå">>),
		{ok,[Treated]} = Mod:list_dir_all("."),
		ok = Mod:delete(<<"ååå">>),
		{ok,[]} = Mod:list_dir("."),
		ok = Mod:set_cwd(".."),
		rm_r2(Mod,"åäö_dir");
	    false ->
		ok
	end,

	_ = make_icky_dir(Mod, treat_icky(<<"åäö_dir">>)),
	if 
	    UniMode and (OS =/= win32) ->
		{error,enoent} = Mod:set_cwd("åäö_dir");
	    true ->
		ok
	end,
	ok = Mod:set_cwd(".."),
	{ok,BeginAt} = Mod:get_cwd(),
	case Mod:make_link("fil1", "nisseö") of
	    ok ->
		{ok, <<"fil1">>} = Mod:read_file("nisseö"),
		{ok, #file_info{type = regular}} = Mod:read_link_info("nisseö"),
		ok = Mod:delete("nisseö"),
		ok = Mod:make_link("fil1",treat_icky(<<"nisseö">>)),
		{ok, <<"fil1">>} = Mod:read_file(treat_icky(<<"nisseö">>)),
		{ok, #file_info{type = regular}} = Mod:read_link_info(treat_icky(<<"nisseö">>)),
		ok = Mod:delete(treat_icky(<<"nisseö">>)),
		{ok, <<"fil1">>} = Mod:read_file("fil1"),
		{error,enoent} = Mod:read_file("nisseö"),
		{error,enoent} = Mod:read_link_info("nisseö"),
		{error,enoent} = Mod:read_file(treat_icky(<<"nisseö">>)),
		{error,enoent} = Mod:read_link_info(treat_icky(<<"nisseö">>));
	    {error,enotsup} ->
		ok
	end,
	[ begin
	      {ok, FD} = Mod:open(Name,[read]),
	      {ok, Content} = Mod:read(FD,1024),
	      ok = file:close(FD)
	  end || {regular,Name,Content} <- IckyDir ],
	[ begin
	      {ok, FD} = Mod:open(Name,[read,binary]),
	      BC = list_to_binary([Content]),
	      {ok, BC} = Mod:read(FD,1024),
	      ok = file:close(FD)
	  end || {regular,Name,Content} <- IckyDir ],
        Mod:rename("åäö2","åäö_fil1"),
        {ok, <<"åäö2">>} = Mod:read_file("åäö_fil1"),
        {error,enoent} = Mod:read_file("åäö2"),
        Mod:rename("åäö_fil1","åäö2"),
        {ok, <<"åäö2">>} = Mod:read_file("åäö2"),
        {error,enoent} = Mod:read_file("åäö_fil1"),

        Mod:rename("åäö2",treat_icky(<<"åäö_fil1">>)),
        {ok, <<"åäö2">>} = Mod:read_file(treat_icky(<<"åäö_fil1">>)),
	if
	    UniMode and (OS =/= win32) ->
		{error,enoent} = Mod:read_file("åäö_fil1");
	    true ->
		ok
	end,
        {error,enoent} = Mod:read_file("åäö2"),
        Mod:rename(treat_icky(<<"åäö_fil1">>),"åäö2"),
        {ok, <<"åäö2">>} = Mod:read_file("åäö2"),
        {error,enoent} = Mod:read_file("åäö_fil1"),
        {error,enoent} = Mod:read_file(treat_icky(<<"åäö_fil1">>)),

        {ok,FI} = Mod:read_file_info("åäö2"),
	NewMode = FI#file_info.mode band (bnot 8#333),
	NewMode2 = NewMode bor 8#222,
	true = NewMode2 =/= NewMode,
        ok = Mod:write_file_info("åäö2",FI#file_info{mode = NewMode}),
        {ok,#file_info{mode = NewMode}} = Mod:read_file_info("åäö2"),
        ok = Mod:write_file_info("åäö2",FI#file_info{mode = NewMode2}),
        {ok,#file_info{mode = NewMode2}} = Mod:read_file_info("åäö2"),

        {ok,FII} = Mod:read_file_info(treat_icky(<<"åäö5">>)),
	true = NewMode2 =/= NewMode,
        ok = Mod:write_file_info(treat_icky(<<"åäö5">>),FII#file_info{mode = NewMode}),
        {ok,#file_info{mode = NewMode}} = Mod:read_file_info(treat_icky(<<"åäö5">>)),
        ok = Mod:write_file_info(<<"åäö5">>,FII#file_info{mode = NewMode2}),
        {ok,#file_info{mode = NewMode2}} = Mod:read_file_info(treat_icky(<<"åäö5">>)),
	ok
    after
	Mod:set_cwd(Dir),
        io:format("Wd now: ~s~n",[Dir])
    end.

check_very_icky(Mod) -> 
    {ok,Dir} = Mod:get_cwd(),
    try
	true=(length("åäö") =:= 3),
	UniMode = file:native_name_encoding() =/= latin1,
	if
	    not UniMode ->
		throw(need_unicode_mode);
	    true ->
		ok
	end,
	VeryIckyDir = make_very_icky_dir(Mod, "very_icky_dir"),
	Expected = lists:sort(convlist(list(VeryIckyDir))),
	{ok, Actual} = Mod:list_dir_all("."),
	Expected = lists:sort(Actual),
	{ok,D2} = Mod:get_cwd(),
	true = is_list(D2),
	[ true = ((is_list(El) or is_binary(El)))  || El <- Expected],
	Syms = [{S,conv(Targ),list_to_binary(get_data(Targ, VeryIckyDir))}
		|| {symlink,S,Targ} <- VeryIckyDir],
	[ {ok, Cont} = Mod:read_file(SymL) || {SymL,_,Cont} <- Syms ],
	[ {ok, Targ} = fixlink(Mod:read_link_all(SymL)) ||
	    {SymL,Targ,_} <- Syms ],

	{ok,BeginAt} = Mod:get_cwd(),
	OtherDir = [1088,1079,1091] ++ "_dir",
	true = is_list(BeginAt),
	make_very_icky_dir(Mod, OtherDir),
        {error,enoent} = Mod:set_cwd("åä_dir"),
	{ok, NowAt} = Mod:get_cwd(),
	true = is_list(NowAt),
	true = BeginAt =/= NowAt,
	ok = Mod:set_cwd(".."),
	{ok,BeginAt} = Mod:get_cwd(),
	rm_r2(Mod, OtherDir),

	case Mod:make_link("fil1","nisse"++[1088,1079,1091]) of
	    ok ->
		{ok, <<"fil1">>} =
		    Mod:read_file("nisse"++[1088,1079,1091]),
		{ok, #file_info{type = regular}} =
		    Mod:read_link_info("nisse"++[1088,1079,1091]),
		ok = Mod:delete("nisse"++[1088,1079,1091]),
		ok = Mod:make_link("fil1",<<"nisseö">>),
		{ok, <<"fil1">>} = Mod:read_file(<<"nisseö">>),
		{ok, #file_info{type = regular}} =
		    Mod:read_link_info(<<"nisseö">>),
		ok = Mod:delete(<<"nisseö">>),
		{ok, <<"fil1">>} = Mod:read_file("fil1"),
		{error,enoent} = Mod:read_file("nisse"++[1088,1079,1091]),
		{error,enoent} = Mod:read_link_info("nisse"++[1088,1079,1091]),
		{error,enoent} = Mod:read_file(<<"nisseö">>),
		{error,enoent} = Mod:read_link_info(<<"nisseö">>);
	    {error,enotsup} ->
		ok
	end,
	[ begin
	      {ok, FD} = Mod:open(Name,[read]),
	      {ok, Content} = Mod:read(FD,1024),
	      ok = file:close(FD)
	  end || {regular,Name,Content} <- VeryIckyDir ],
	[ begin
	      {ok, FD} = Mod:open(Name,[read,binary]),
	      BC = list_to_binary([Content]),
	      {ok, BC} = Mod:read(FD,1024),
	      ok = file:close(FD)
	  end || {regular,Name,Content} <- VeryIckyDir ],
	Mod:rename([956,965,963,954,959,49],
		   [956,965,963,954,959]++"_fil1"),
        {ok, <<"åäö2">>} = Mod:read_file([956,965,963,954,959]++"_fil1"),
	{error,enoent} = Mod:read_file([956,965,963,954,959,49]),
	Mod:rename([956,965,963,954,959]++"_fil1",[956,965,963,954,959,49]),
        {ok, <<"åäö2">>} = Mod:read_file([956,965,963,954,959,49]),
	{error,enoent} = Mod:read_file([956,965,963,954,959]++"_fil1"),

	{ok,FI} = Mod:read_file_info([956,965,963,954,959,49]),
	NewMode = FI#file_info.mode band (bnot 8#333),
	NewMode2 = NewMode bor 8#222,
	true = NewMode2 =/= NewMode,
	ok = Mod:write_file_info([956,965,963,954,959,49],
				 FI#file_info{mode = NewMode}),
	{ok,#file_info{mode = NewMode}} =
	    Mod:read_file_info([956,965,963,954,959,49]),
	ok = Mod:write_file_info([956,965,963,954,959,49],
				 FI#file_info{mode = NewMode2}),
	{ok,#file_info{mode = NewMode2}} =
	    Mod:read_file_info([956,965,963,954,959,49]),
	ok
    catch
	throw:need_unicode_mode ->
	    io:format("Sorry, can only run in unicode mode.~n"),
	    need_unicode_mode
    after
	Mod:set_cwd(Dir),
        io:format("Wd now: ~s~n",[Dir])
    end.

%%
%% Utilities
%%


rm_rf(Mod,Dir) ->
    case  Mod:read_link_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    {ok, Content} = Mod:list_dir_all(Dir),
	    [ rm_rf(Mod,filename:join(Dir,C)) || C <- Content ],
	    Mod:del_dir(Dir),
	    ok;
	{ok, #file_info{}} ->
	    Mod:delete(Dir);
	_ ->
	    ok
    end.

rm_r(Mod,Dir) ->
    case  Mod:read_link_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    {ok,#file_info{type = directory}} =  Mod:read_file_info(Dir),
	    {ok, Content} = Mod:list_dir_all(Dir),
	    [ true = is_list(Part) || Part <- Content ],
	    [ true = is_list(filename:join(Dir,Part)) || Part <- Content ],
	    [ rm_r(Mod,filename:join(Dir,C)) || C <- Content ],
	    ok = Mod:del_dir(Dir),
	    ok;
	{ok, #file_info{type = regular}} ->
	    {ok,#file_info{type = regular}} =  Mod:read_file_info(Dir),
	    ok = Mod:delete(Dir);
	{ok, #file_info{type = symlink}} ->
	    ok = Mod:delete(Dir)
    end.
%% For icky test, allow binaries sometimes
rm_r2(Mod,Dir) ->
    %% erlang:display({rm_r2,Dir}),
    case  Mod:read_link_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    {ok,#file_info{type = directory}} =  Mod:read_file_info(Dir),
	    {ok, Content} = Mod:list_dir_all(Dir),
	    UniMode = file:native_name_encoding() =/= latin1,
	    [ true = (is_list(Part) orelse UniMode) || Part <- Content ],
	    [ true = (is_list(filename:join(Dir,Part)) orelse UniMode) || Part <- Content ],
	    [ rm_r2(Mod,filename:join(Dir,C)) || C <- Content ],
	    ok = Mod:del_dir(Dir),
	    ok;
	{ok, #file_info{type = regular}} ->
	    {ok,#file_info{type = regular}} =  Mod:read_file_info(Dir),
	    ok = Mod:delete(Dir);
	{ok, #file_info{type = symlink}} ->
	    ok = Mod:delete(Dir)
    end.

make_normal_dir(Mod, DirName) ->
    Dir = [{regular,"fil1","fil1"},
	   {regular,"fil2","fil2"},
	   {hardlink,"fil3","fil2"},
	   {symlink,"fil4","fil2"},
	   {directory,"subdir",
	    [{regular,"subfil1","subfil1"}]}],
    rm_rf(Mod, DirName),
    Mod:make_dir(DirName),
    Mod:set_cwd(DirName),
    make_dir_contents(Dir, Mod).

make_icky_dir(Mod, IckyDirName) ->
    Icky = [{regular,"fil1","fil1"},
	    {regular,"åäö2","åäö2"},
	    {hardlink,"åäö3","åäö2"},
	    {symlink,"åäö4","åäö2"},
	    {regular,treat_icky(<<"åäö5">>),"åäö5"},
	    {symlink,treat_icky(<<"åäö6">>),treat_icky(<<"åäö5">>)},
	    {directory,treat_icky(<<"åäösubdir2">>),
	     [{regular,treat_icky(<<"åäösubfil2">>),"åäösubfil12"},
	      {regular,"åäösubfil3","åäösubfil13"}]},
	    {directory,"åäösubdir",
	     [{regular,"åäösubfil1","åäösubfil1"}]}],
    rm_rf(Mod, IckyDirName),
    ok = Mod:make_dir(IckyDirName),
    ok = Mod:set_cwd(IckyDirName),
    make_dir_contents(Icky, Mod).

hopeless_darwin() ->
    case {os:type(),os:version()} of
	{{unix,darwin},{Major,_,_}} when Major < 9 ->
	    true;
	_ ->
	    false
    end.

make_very_icky_dir(Mod, DirName) ->
    Desc = [{regular,"fil1","fil1"},
	    {regular,[956,965,963,954,959,49],"åäö2"},
	    {hardlink,[956,965,963,954,959,50],
	     [956,965,963,954,959,49],
	     "åäö2"},
	    {symlink,[956,965,963,954,959,51],[956,965,963,954,959,49]},
	    {regular,treat_icky(<<"åäö5">>),"åäö5"},
	    {symlink,treat_icky(<<"åäö6">>),treat_icky(<<"åäö5">>)},
	    {directory,treat_icky(<<"åäösubdir2">>),
	     [{regular,treat_icky(<<"åäösubfil2">>),"åäösubfil12"},
	      {regular,"åäösubfil3","åäösubfil13"}]},
	    {directory,[956,965,963,954,959]++"subdir1",
	     [{regular,[956,965,963,954,959]++"subfil1","åäösubfil1"}]}],
    rm_rf(Mod, DirName),
    ok = Mod:make_dir(DirName),
    ok = Mod:set_cwd(DirName),
    make_dir_contents(Desc, Mod).

%% Some OS'es simply do not allow non UTF8 filenames
treat_icky(Bin) ->
    case os:type() of
	{unix,darwin} ->
	    binary_to_list(procentify(Bin));
	{win32,_} ->
	    binary_to_list(Bin);
	_ ->
	    Bin
    end.

%% Handle windows having absolute soft link targets.
fixlink({ok,Link}) ->
    case os:type() of
	{win32,_} ->
	    {ok,filename:basename(Link)};
	_ ->
	    {ok,Link}
    end;
fixlink(X) ->
    X.

procentify(<<>>) ->
    <<>>;
procentify(<<X:8,Rst/binary>>) when X > 127 ->
    T=procentify(Rst),
    Y = list_to_binary([$% 
			| io_lib:format("~2.16B",[X])]),
    <<Y/binary,T/binary>>;
procentify(<<X:8,Rst/binary>>) ->
    T=procentify(Rst),
    <<X:8,T/binary>>.


list([]) ->
    [];
list([{_,Name,_} | T]) ->
    [Name | list(T)].


get_data(FN,List) ->
    case lists:keysearch(FN,2,List) of
	{value,{regular,FN,C}} ->
	    C;
	{value,{symlink,FN,NewFN}} ->
	    get_data(NewFN,List);
	_->
	    []
    end.


convlist(L) ->
    convlist(file:native_name_encoding(),L).
convlist(latin1,[Bin|T]) when is_binary(Bin) ->
    %% erlang:display('Convert...'),
    [binary_to_list(Bin)| convlist(latin1,T)];
convlist(Any,[H|T]) ->
    [H|convlist(Any,T)];
convlist(_,[]) ->
    [].

conv(L) ->
    NoUniMode = file:native_name_encoding() =:= latin1,
    if 
	NoUniMode, is_binary(L) ->
	    binary_to_list(L);
	true ->
	    L
    end.


make_dir_contents([{regular,Name,Contents}=H|T], Mod) ->
    ok = Mod:write_file(Name, Contents),
    [H|make_dir_contents(T, Mod)];
make_dir_contents([{hardlink,Target,Name}|T], Mod) ->
    case Mod:make_link(Name, Target) of
	ok ->
	    [{regular,Target,Name}|make_dir_contents(T, Mod)];
	{error,enotsup} ->
	    make_dir_contents(T, Mod)
    end;
make_dir_contents([{hardlink,Target,Name,Contents}|T], Mod) ->
    case Mod:make_link(Name, Target) of
	ok ->
	    [{regular,Target,Contents}|make_dir_contents(T, Mod)];
	{error,enotsup} ->
	    make_dir_contents(T, Mod)
    end;
make_dir_contents([{symlink,Target,Name}=H|T], Mod) ->
    case Mod:make_symlink(Name, Target) of
	ok ->
	    [H|make_dir_contents(T, Mod)];
	{error,enotsup} ->
	    make_dir_contents(T, Mod);
	{error,eperm} ->
	    make_dir_contents(T, Mod)
    end;
make_dir_contents([{directory,Dir,C0}|T], Mod) ->
    ok = Mod:make_dir(Dir),
    C1 = [case Op of
	      Link when Link =:= hardlink; Link =:= symlink ->
		  {Op,filename:join(Dir, Name0),filename:join(Dir, Extra)};
	      _ ->
		  {Op,filename:join(Dir, Name0),Extra}
	  end || {Op,Name0,Extra} <- C0],
    C2 = make_dir_contents(C1, Mod),
    C = [{Op,filename:basename(Name0),Extra} ||
	    {Op,Name0,Extra} <- C2],
    [{directory,Dir,C}|make_dir_contents(T, Mod)];
make_dir_contents([], _Mod) ->
    [].


rand_comp_decomp(Max) ->
    N = rand:uniform(Max),
    L = [ rand_decomp() || _ <- lists:seq(1,N) ],
    LC = [ A || {A,_} <- L],
    LD = lists:flatten([B || {_,B} <- L]),
    LB = unicode:characters_to_binary(LD,unicode,utf8),
    {LC,LB}.

rand_decomp() ->
    BT = bigtup(),
    SZ = tuple_size(BT),
    element(rand:uniform(SZ),BT).
bigtup() ->
    {{192,[65,768]},
     {200,[69,768]},
     {204,[73,768]},
     {210,[79,768]},
     {217,[85,768]},
     {7808,[87,768]},
     {7922,[89,768]},
     {224,[97,768]},
     {232,[101,768]},
     {236,[105,768]},
     {242,[111,768]},
     {249,[117,768]},
     {7809,[119,768]},
     {7923,[121,768]},
     {8173,[168,768]},
     {7846,[65,770,768]},
     {7872,[69,770,768]},
     {7890,[79,770,768]},
     {7847,[97,770,768]},
     {7873,[101,770,768]},
     {7891,[111,770,768]},
     {7700,[69,772,768]},
     {7760,[79,772,768]},
     {7701,[101,772,768]},
     {7761,[111,772,768]},
     {7856,[65,774,768]},
     {7857,[97,774,768]},
     {475,[85,776,768]},
     {476,[117,776,768]},
     {8146,[953,776,768]},
     {8162,[965,776,768]},
     {8074,[913,837,787,768]},
     {8090,[919,837,787,768]},
     {8106,[937,837,787,768]},
     {8066,[945,837,787,768]},
     {8082,[951,837,787,768]},
     {8098,[969,837,787,768]},
     {7946,[913,787,768]},
     {7962,[917,787,768]},
     {7978,[919,787,768]},
     {7994,[921,787,768]},
     {8010,[927,787,768]},
     {8042,[937,787,768]},
     {7938,[945,787,768]},
     {7954,[949,787,768]},
     {7970,[951,787,768]},
     {7986,[953,787,768]},
     {8002,[959,787,768]},
     {8018,[965,787,768]},
     {8034,[969,787,768]},
     {8075,[913,837,788,768]},
     {8091,[919,837,788,768]},
     {8107,[937,837,788,768]},
     {8067,[945,837,788,768]},
     {8083,[951,837,788,768]},
     {8099,[969,837,788,768]},
     {7947,[913,788,768]},
     {7963,[917,788,768]},
     {7979,[919,788,768]},
     {7995,[921,788,768]},
     {8011,[927,788,768]},
     {8027,[933,788,768]},
     {8043,[937,788,768]},
     {7939,[945,788,768]},
     {7955,[949,788,768]},
     {7971,[951,788,768]},
     {7987,[953,788,768]},
     {8003,[959,788,768]},
     {8019,[965,788,768]},
     {8035,[969,788,768]},
     {7900,[79,795,768]},
     {7914,[85,795,768]},
     {7901,[111,795,768]},
     {7915,[117,795,768]},
     {8114,[945,837,768]},
     {8130,[951,837,768]},
     {8178,[969,837,768]},
     {8122,[913,768]},
     {8136,[917,768]},
     {8138,[919,768]},
     {8154,[921,768]},
     {8184,[927,768]},
     {8170,[933,768]},
     {8186,[937,768]},
     {8048,[945,768]},
     {8050,[949,768]},
     {8052,[951,768]},
     {8054,[953,768]},
     {8056,[959,768]},
     {8058,[965,768]},
     {8060,[969,768]},
     {8141,[8127,768]},
     {8157,[8190,768]},
     {193,[65,769]},
     {262,[67,769]},
     {201,[69,769]},
     {500,[71,769]},
     {205,[73,769]},
     {7728,[75,769]},
     {313,[76,769]},
     {7742,[77,769]},
     {323,[78,769]},
     {211,[79,769]},
     {7764,[80,769]},
     {340,[82,769]},
     {346,[83,769]},
     {218,[85,769]},
     {7810,[87,769]},
     {221,[89,769]},
     {377,[90,769]},
     {225,[97,769]},
     {263,[99,769]},
     {233,[101,769]},
     {501,[103,769]},
     {237,[105,769]},
     {7729,[107,769]},
     {314,[108,769]},
     {7743,[109,769]},
     {324,[110,769]},
     {243,[111,769]},
     {7765,[112,769]},
     {341,[114,769]},
     {347,[115,769]},
     {250,[117,769]},
     {7811,[119,769]},
     {253,[121,769]},
     {378,[122,769]},
     {8174,[168,769]},
     {508,[198,769]},
     {510,[216,769]},
     {509,[230,769]},
     {511,[248,769]},
     {7844,[65,770,769]},
     {7870,[69,770,769]},
     {7888,[79,770,769]},
     {7845,[97,770,769]},
     {7871,[101,770,769]},
     {7889,[111,770,769]},
     {7756,[79,771,769]},
     {7800,[85,771,769]},
     {7757,[111,771,769]},
     {7801,[117,771,769]},
     {7702,[69,772,769]},
     {7762,[79,772,769]},
     {7703,[101,772,769]},
     {7763,[111,772,769]},
     {7854,[65,774,769]},
     {7855,[97,774,769]},
     {7726,[73,776,769]},
     {471,[85,776,769]},
     {7727,[105,776,769]},
     {472,[117,776,769]},
     {8147,[953,776,769]},
     {8163,[965,776,769]},
     {506,[65,778,769]},
     {507,[97,778,769]},
     {8076,[913,837,787,769]},
     {8092,[919,837,787,769]},
     {8108,[937,837,787,769]},
     {8068,[945,837,787,769]},
     {8084,[951,837,787,769]},
     {8100,[969,837,787,769]},
     {7948,[913,787,769]},
     {7964,[917,787,769]},
     {7980,[919,787,769]},
     {7996,[921,787,769]},
     {8012,[927,787,769]},
     {8044,[937,787,769]},
     {7940,[945,787,769]},
     {7956,[949,787,769]},
     {7972,[951,787,769]},
     {7988,[953,787,769]},
     {8004,[959,787,769]},
     {8020,[965,787,769]},
     {8036,[969,787,769]},
     {8077,[913,837,788,769]},
     {8093,[919,837,788,769]},
     {8109,[937,837,788,769]},
     {8069,[945,837,788,769]},
     {8085,[951,837,788,769]},
     {8101,[969,837,788,769]},
     {7949,[913,788,769]},
     {7965,[917,788,769]},
     {7981,[919,788,769]},
     {7997,[921,788,769]},
     {8013,[927,788,769]},
     {8029,[933,788,769]},
     {8045,[937,788,769]},
     {7941,[945,788,769]},
     {7957,[949,788,769]},
     {7973,[951,788,769]},
     {7989,[953,788,769]},
     {8005,[959,788,769]},
     {8021,[965,788,769]},
     {8037,[969,788,769]},
     {7898,[79,795,769]},
     {7912,[85,795,769]},
     {7899,[111,795,769]},
     {7913,[117,795,769]},
     {7688,[67,807,769]},
     {7689,[99,807,769]},
     {8116,[945,837,769]},
     {8132,[951,837,769]},
     {8180,[959,837,769]},
     {8123,[913,769]},
     {8137,[917,769]},
     {8139,[919,769]},
     {8155,[921,769]},
     {8185,[927,769]},
     {8171,[933,769]},
     {8187,[937,769]},
     {8049,[945,769]},
     {8051,[949,769]},
     {8053,[951,769]},
     {8055,[953,769]},
     {8057,[959,769]},
     {8059,[965,769]},
     {8061,[969,769]},
     {1027,[1043,769]},
     {1036,[1050,769]},
     {1107,[1075,769]},
     {1116,[1082,769]},
     {8142,[8127,769]},
     {8158,[8190,769]},
     {194,[65,770]},
     {264,[67,770]},
     {202,[69,770]},
     {284,[71,770]},
     {292,[72,770]},
     {206,[73,770]},
     {308,[74,770]},
     {212,[79,770]},
     {348,[83,770]},
     {219,[85,770]},
     {372,[87,770]},
     {374,[89,770]},
     {7824,[90,770]},
     {226,[97,770]},
     {265,[99,770]},
     {234,[101,770]},
     {285,[103,770]},
     {293,[104,770]},
     {238,[105,770]},
     {309,[106,770]},
     {244,[111,770]},
     {349,[115,770]},
     {251,[117,770]},
     {373,[119,770]},
     {375,[121,770]},
     {7825,[122,770]},
     {7852,[65,803,770]},
     {7878,[69,803,770]},
     {7896,[79,803,770]},
     {7853,[97,803,770]},
     {7879,[101,803,770]},
     {7897,[111,803,770]},
     {195,[65,771]},
     {7868,[69,771]},
     {296,[73,771]},
     {209,[78,771]},
     {213,[79,771]},
     {360,[85,771]},
     {7804,[86,771]},
     {7928,[89,771]},
     {227,[97,771]},
     {7869,[101,771]},
     {297,[105,771]},
     {241,[110,771]},
     {245,[111,771]},
     {361,[117,771]},
     {7805,[118,771]},
     {7929,[121,771]},
     {7850,[65,770,771]},
     {7876,[69,770,771]},
     {7894,[79,770,771]},
     {7851,[97,770,771]},
     {7877,[101,770,771]},
     {7895,[111,770,771]},
     {7860,[65,774,771]},
     {7861,[97,774,771]},
     {7904,[79,795,771]},
     {7918,[85,795,771]},
     {7905,[111,795,771]},
     {7919,[117,795,771]},
     {256,[65,772]},
     {274,[69,772]},
     {7712,[71,772]},
     {298,[73,772]},
     {332,[79,772]},
     {362,[85,772]},
     {257,[97,772]},
     {275,[101,772]},
     {7713,[103,772]},
     {299,[105,772]},
     {333,[111,772]},
     {363,[117,772]},
     {482,[198,772]},
     {483,[230,772]},
     {480,[65,775,772]},
     {481,[97,775,772]},
     {478,[65,776,772]},
     {469,[85,776,772]},
     {479,[97,776,772]},
     {470,[117,776,772]},
     {7736,[76,803,772]},
     {7772,[82,803,772]},
     {7737,[108,803,772]},
     {7773,[114,803,772]},
     {492,[79,808,772]},
     {493,[111,808,772]},
     {8121,[913,772]},
     {8153,[921,772]},
     {8169,[933,772]},
     {8113,[945,772]},
     {8145,[953,772]},
     {8161,[965,772]},
     {1250,[1048,772]},
     {1262,[1059,772]},
     {1251,[1080,772]},
     {1263,[1091,772]},
     {258,[65,774]},
     {276,[69,774]},
     {286,[71,774]},
     {300,[73,774]},
     {334,[79,774]},
     {364,[85,774]},
     {259,[97,774]},
     {277,[101,774]},
     {287,[103,774]},
     {301,[105,774]},
     {335,[111,774]},
     {365,[117,774]},
     {7862,[65,803,774]},
     {7863,[97,803,774]},
     {7708,[69,807,774]},
     {7709,[101,807,774]},
     {8120,[913,774]},
     {8152,[921,774]},
     {8168,[933,774]},
     {8112,[945,774]},
     {8144,[953,774]},
     {8160,[965,774]},
     {1232,[1040,774]},
     {1238,[1045,774]},
     {1217,[1046,774]},
     {1049,[1048,774]},
     {1038,[1059,774]},
     {1233,[1072,774]},
     {1239,[1077,774]},
     {1218,[1078,774]},
     {1081,[1080,774]},
     {1118,[1091,774]},
     {7682,[66,775]},
     {266,[67,775]},
     {7690,[68,775]},
     {278,[69,775]},
     {7710,[70,775]},
     {288,[71,775]},
     {7714,[72,775]},
     {304,[73,775]},
     {7744,[77,775]},
     {7748,[78,775]},
     {7766,[80,775]},
     {7768,[82,775]},
     {7776,[83,775]},
     {7786,[84,775]},
     {7814,[87,775]},
     {7818,[88,775]},
     {7822,[89,775]},
     {379,[90,775]},
     {7683,[98,775]},
     {267,[99,775]},
     {7691,[100,775]},
     {279,[101,775]},
     {7711,[102,775]},
     {289,[103,775]},
     {7715,[104,775]},
     {7745,[109,775]},
     {7749,[110,775]},
     {7767,[112,775]},
     {7769,[114,775]},
     {7777,[115,775]},
     {7787,[116,775]},
     {7815,[119,775]},
     {7819,[120,775]},
     {7823,[121,775]},
     {380,[122,775]},
     {7835,[383,775]},
     {7780,[83,769,775]},
     {7781,[115,769,775]},
     {784,[774,775]},
     {7782,[83,780,775]},
     {7783,[115,780,775]},
     {7784,[83,803,775]},
     {7785,[115,803,775]},
     {196,[65,776]},
     {203,[69,776]},
     {7718,[72,776]},
     {207,[73,776]},
     {214,[79,776]},
     {220,[85,776]},
     {7812,[87,776]},
     {7820,[88,776]},
     {376,[89,776]},
     {228,[97,776]},
     {235,[101,776]},
     {7719,[104,776]},
     {239,[105,776]},
     {246,[111,776]},
     {7831,[116,776]},
     {252,[117,776]},
     {7813,[119,776]},
     {7821,[120,776]},
     {255,[121,776]},
     {1242,[399,776]},
     {1258,[415,776]},
     {1243,[601,776]},
     {1259,[629,776]},
     {7758,[79,771,776]},
     {7759,[111,771,776]},
     {7802,[85,772,776]},
     {7803,[117,772,776]},
     {938,[921,776]},
     {939,[933,776]},
     {970,[953,776]},
     {971,[965,776]},
     {980,[978,776]},
     {1031,[1030,776]},
     {1234,[1040,776]},
     {1025,[1045,776]},
     {1244,[1046,776]},
     {1246,[1047,776]},
     {1252,[1048,776]},
     {1254,[1054,776]},
     {1264,[1059,776]},
     {1268,[1063,776]},
     {1272,[1067,776]},
     {1235,[1072,776]},
     {1105,[1077,776]},
     {1245,[1078,776]},
     {1247,[1079,776]},
     {1253,[1080,776]},
     {1255,[1086,776]},
     {1265,[1091,776]},
     {1269,[1095,776]},
     {1273,[1099,776]},
     {1111,[1110,776]},
     {7842,[65,777]},
     {7866,[69,777]},
     {7880,[73,777]},
     {7886,[79,777]},
     {7910,[85,777]},
     {7926,[89,777]},
     {7843,[97,777]},
     {7867,[101,777]},
     {7881,[105,777]},
     {7887,[111,777]},
     {7911,[117,777]},
     {7927,[121,777]},
     {7848,[65,770,777]},
     {7874,[69,770,777]},
     {7892,[79,770,777]},
     {7849,[97,770,777]},
     {7875,[101,770,777]},
     {7893,[111,770,777]},
     {7858,[65,774,777]},
     {7859,[97,774,777]},
     {7902,[79,795,777]},
     {7916,[85,795,777]},
     {7903,[111,795,777]},
     {7917,[117,795,777]},
     {197,[65,778]},
     {366,[85,778]},
     {229,[97,778]},
     {367,[117,778]},
     {7832,[119,778]},
     {7833,[121,778]},
     {336,[79,779]},
     {368,[85,779]},
     {337,[111,779]},
     {369,[117,779]},
     {1266,[1059,779]},
     {1267,[1091,779]},
     {461,[65,780]},
     {268,[67,780]},
     {270,[68,780]},
     {282,[69,780]},
     {486,[71,780]},
     {463,[73,780]},
     {488,[75,780]},
     {317,[76,780]},
     {327,[78,780]},
     {465,[79,780]},
     {344,[82,780]},
     {352,[83,780]},
     {356,[84,780]},
     {467,[85,780]},
     {381,[90,780]},
     {462,[97,780]},
     {269,[99,780]},
     {271,[100,780]},
     {283,[101,780]},
     {487,[103,780]},
     {464,[105,780]},
     {496,[106,780]},
     {489,[107,780]},
     {318,[108,780]},
     {328,[110,780]},
     {466,[111,780]},
     {345,[114,780]},
     {353,[115,780]},
     {357,[116,780]},
     {468,[117,780]},
     {382,[122,780]},
     {494,[439,780]},
     {495,[658,780]},
     {473,[85,776,780]},
     {474,[117,776,780]},
     {901,[168,781]},
     {912,[953,776,781]},
     {944,[965,776,781]},
     {902,[913,781]},
     {904,[917,781]},
     {905,[919,781]},
     {906,[921,781]},
     {908,[927,781]},
     {910,[933,781]},
     {911,[937,781]},
     {940,[945,781]},
     {941,[949,781]},
     {942,[951,781]},
     {943,[953,781]},
     {972,[959,781]},
     {973,[965,781]},
     {974,[969,781]},
     {979,[978,781]},
     {512,[65,783]},
     {516,[69,783]},
     {520,[73,783]},
     {524,[79,783]},
     {528,[82,783]},
     {532,[85,783]},
     {513,[97,783]},
     {517,[101,783]},
     {521,[105,783]},
     {525,[111,783]},
     {529,[114,783]},
     {533,[117,783]},
     {1142,[1140,783]},
     {1143,[1141,783]},
     {514,[65,785]},
     {518,[69,785]},
     {522,[73,785]},
     {526,[79,785]},
     {530,[82,785]},
     {534,[85,785]},
     {515,[97,785]},
     {519,[101,785]},
     {523,[105,785]},
     {527,[111,785]},
     {531,[114,785]},
     {535,[117,785]},
     {8072,[913,837,787]},
     {8088,[919,837,787]},
     {8104,[937,837,787]},
     {8064,[945,837,787]},
     {8080,[951,837,787]},
     {8096,[969,837,787]},
     {7944,[913,787]},
     {7960,[917,787]},
     {7976,[919,787]},
     {7992,[921,787]},
     {8008,[927,787]},
     {8040,[937,787]},
     {7936,[945,787]},
     {7952,[949,787]},
     {7968,[951,787]},
     {7984,[953,787]},
     {8000,[959,787]},
     {8164,[961,787]},
     {8016,[965,787]},
     {8032,[969,787]},
     {8073,[913,837,788]},
     {8089,[919,837,788]},
     {8105,[937,837,788]},
     {8065,[945,837,788]},
     {8081,[951,837,788]},
     {8097,[969,837,788]},
     {7945,[913,788]},
     {7961,[917,788]},
     {7977,[919,788]},
     {7993,[921,788]},
     {8009,[927,788]},
     {8172,[929,788]},
     {8025,[933,788]},
     {8041,[937,788]},
     {7937,[945,788]},
     {7953,[949,788]},
     {7969,[951,788]},
     {7985,[953,788]},
     {8001,[959,788]},
     {8165,[961,788]},
     {8017,[965,788]},
     {8033,[969,788]},
     {416,[79,795]},
     {431,[85,795]},
     {417,[111,795]},
     {432,[117,795]},
     {7840,[65,803]},
     {7684,[66,803]},
     {7692,[68,803]},
     {7864,[69,803]},
     {7716,[72,803]},
     {7882,[73,803]},
     {7730,[75,803]},
     {7734,[76,803]},
     {7746,[77,803]},
     {7750,[78,803]},
     {7884,[79,803]},
     {7770,[82,803]},
     {7778,[83,803]},
     {7788,[84,803]},
     {7908,[85,803]},
     {7806,[86,803]},
     {7816,[87,803]},
     {7924,[89,803]},
     {7826,[90,803]},
     {7841,[97,803]},
     {7685,[98,803]},
     {7693,[100,803]},
     {7865,[101,803]},
     {7717,[104,803]},
     {7883,[105,803]},
     {7731,[107,803]},
     {7735,[108,803]},
     {7747,[109,803]},
     {7751,[110,803]},
     {7885,[111,803]},
     {7771,[114,803]},
     {7779,[115,803]},
     {7789,[116,803]},
     {7909,[117,803]},
     {7807,[118,803]},
     {7817,[119,803]},
     {7925,[121,803]},
     {7827,[122,803]},
     {7906,[79,795,803]},
     {7920,[85,795,803]},
     {7907,[111,795,803]},
     {7921,[117,795,803]},
     {7794,[85,804]},
     {7795,[117,804]},
     {7680,[65,805]},
     {7681,[97,805]},
     {199,[67,807]},
     {7696,[68,807]},
     {290,[71,807]},
     {7720,[72,807]},
     {310,[75,807]},
     {315,[76,807]},
     {325,[78,807]},
     {342,[82,807]},
     {350,[83,807]},
     {354,[84,807]},
     {231,[99,807]},
     {7697,[100,807]},
     {291,[103,807]},
     {7721,[104,807]},
     {311,[107,807]},
     {316,[108,807]},
     {326,[110,807]},
     {343,[114,807]},
     {351,[115,807]},
     {355,[116,807]},
     {260,[65,808]},
     {280,[69,808]},
     {302,[73,808]},
     {490,[79,808]},
     {370,[85,808]},
     {261,[97,808]},
     {281,[101,808]},
     {303,[105,808]},
     {491,[111,808]},
     {371,[117,808]},
     {7698,[68,813]},
     {7704,[69,813]},
     {7740,[76,813]},
     {7754,[78,813]},
     {7792,[84,813]},
     {7798,[85,813]},
     {7699,[100,813]},
     {7705,[101,813]},
     {7741,[108,813]},
     {7755,[110,813]},
     {7793,[116,813]},
     {7799,[117,813]},
     {7722,[72,814]},
     {7723,[104,814]},
     {7706,[69,816]},
     {7724,[73,816]},
     {7796,[85,816]},
     {7707,[101,816]},
     {7725,[105,816]},
     {7797,[117,816]},
     {7686,[66,817]},
     {7694,[68,817]},
     {7732,[75,817]},
     {7738,[76,817]},
     {7752,[78,817]},
     {7774,[82,817]},
     {7790,[84,817]},
     {7828,[90,817]},
     {7687,[98,817]},
     {7695,[100,817]},
     {7830,[104,817]},
     {7733,[107,817]},
     {7739,[108,817]},
     {7753,[110,817]},
     {7775,[114,817]},
     {7791,[116,817]},
     {7829,[122,817]},
     {8129,[168,834]},
     {8151,[953,776,834]},
     {8167,[965,776,834]},
     {8078,[913,837,787,834]},
     {8094,[919,837,787,834]},
     {8110,[937,837,787,834]},
     {8070,[945,837,787,834]},
     {8086,[951,837,787,834]},
     {8102,[969,837,787,834]},
     {7950,[913,787,834]},
     {7982,[919,787,834]},
     {7998,[921,787,834]},
     {8046,[937,787,834]},
     {7942,[945,787,834]},
     {7974,[951,787,834]},
     {7990,[953,787,834]},
     {8022,[965,787,834]},
     {8038,[969,787,834]},
     {8079,[913,837,788,834]},
     {8095,[919,837,788,834]},
     {8111,[937,837,788,834]},
     {8071,[945,837,788,834]},
     {8087,[951,837,788,834]},
     {8103,[969,837,788,834]},
     {7951,[913,788,834]},
     {7983,[919,788,834]},
     {7999,[921,788,834]},
     {8031,[933,788,834]},
     {8047,[937,788,834]},
     {7943,[945,788,834]},
     {7975,[951,788,834]},
     {7991,[953,788,834]},
     {8023,[965,788,834]},
     {8039,[969,788,834]},
     {8119,[945,837,834]},
     {8135,[951,837,834]},
     {8183,[969,837,834]},
     {8118,[945,834]},
     {8134,[951,834]},
     {8150,[953,834]},
     {8166,[965,834]},
     {8182,[969,834]},
     {8143,[8127,834]},
     {8159,[8190,834]},
     {8124,[913,837]},
     {8140,[919,837]},
     {8188,[937,837]},
     {8115,[945,837]},
     {8131,[951,837]},
     {8179,[969,837]},
     {64302,[1488,1463]},
     {64287,[1522,1463]},
     {64303,[1488,1464]},
     {64331,[1493,1465]},
     {64304,[1488,1468]},
     {64305,[1489,1468]},
     {64306,[1490,1468]},
     {64307,[1491,1468]},
     {64308,[1492,1468]},
     {64309,[1493,1468]},
     {64310,[1494,1468]},
     {64312,[1496,1468]},
     {64313,[1497,1468]},
     {64314,[1498,1468]},
     {64315,[1499,1468]},
     {64316,[1500,1468]},
     {64318,[1502,1468]},
     {64320,[1504,1468]},
     {64321,[1505,1468]},
     {64323,[1507,1468]},
     {64324,[1508,1468]},
     {64326,[1510,1468]},
     {64327,[1511,1468]},
     {64328,[1512,1468]},
     {64329,[1513,1468]},
     {64330,[1514,1468]},
     {64332,[1489,1471]},
     {64333,[1499,1471]},
     {64334,[1508,1471]},
     {64300,[1513,1468,1473]},
     {64298,[1513,1473]},
     {64301,[1513,1468,1474]},
     {64299,[1513,1474]},
     {2392,[2325,2364]},
     {2393,[2326,2364]},
     {2394,[2327,2364]},
     {2395,[2332,2364]},
     {2396,[2337,2364]},
     {2397,[2338,2364]},
     {2345,[2344,2364]},
     {2398,[2347,2364]},
     {2399,[2351,2364]},
     {2353,[2352,2364]},
     {2356,[2355,2364]},
     {2524,[2465,2492]},
     {2525,[2466,2492]},
     {2480,[2476,2492]},
     {2527,[2479,2492]},
     {2507,[2503,2494]},
     {2508,[2503,2519]},
     {2649,[2582,2620]},
     {2650,[2583,2620]},
     {2651,[2588,2620]},
     {2652,[2593,2620]},
     {2654,[2603,2620]},
     {2908,[2849,2876]},
     {2909,[2850,2876]},
     {2911,[2863,2876]},
     {2891,[2887,2878]},
     {2888,[2887,2902]},
     {2892,[2887,2903]},
     {3018,[3014,3006]},
     {3019,[3015,3006]},
     {2964,[2962,3031]},
     {3020,[3014,3031]},
     {3144,[3142,3158]},
     {3274,[3270,3266]},
     {3264,[3263,3285]},
     {3275,[3270,3266,3285]},
     {3271,[3270,3285]},
     {3272,[3270,3286]},
     {3402,[3398,3390]},
     {3403,[3399,3390]},
     {3404,[3398,3415]},
     {3635,[3661,3634]},
     {3763,[3789,3762]},
     {3955,[3954,3953]},
     {3957,[3956,3953]},
     {3959,[4018,3968,3953]},
     {3961,[4019,3968,3953]},
     {3958,[4018,3968]},
     {3960,[4019,3968]},
     {3945,[3904,4021]},
     {4025,[3984,4021]},
     {3907,[3906,4023]},
     {3917,[3916,4023]},
     {3922,[3921,4023]},
     {3927,[3926,4023]},
     {3932,[3931,4023]},
     {3987,[3986,4023]},
     {3997,[3996,4023]},
     {4002,[4001,4023]},
     {4007,[4006,4023]},
     {4012,[4011,4023]},
     {12436,[12358,12441]},
     {12364,[12363,12441]},
     {12366,[12365,12441]},
     {12368,[12367,12441]},
     {12370,[12369,12441]},
     {12372,[12371,12441]},
     {12374,[12373,12441]},
     {12376,[12375,12441]},
     {12378,[12377,12441]},
     {12380,[12379,12441]},
     {12382,[12381,12441]},
     {12384,[12383,12441]},
     {12386,[12385,12441]},
     {12389,[12388,12441]},
     {12391,[12390,12441]},
     {12393,[12392,12441]},
     {12400,[12399,12441]},
     {12403,[12402,12441]},
     {12406,[12405,12441]},
     {12409,[12408,12441]},
     {12412,[12411,12441]},
     {12446,[12445,12441]},
     {12532,[12454,12441]},
     {12460,[12459,12441]},
     {12462,[12461,12441]},
     {12464,[12463,12441]},
     {12466,[12465,12441]},
     {12468,[12467,12441]},
     {12470,[12469,12441]},
     {12472,[12471,12441]},
     {12474,[12473,12441]},
     {12476,[12475,12441]},
     {12478,[12477,12441]},
     {12480,[12479,12441]},
     {12482,[12481,12441]},
     {12485,[12484,12441]},
     {12487,[12486,12441]},
     {12489,[12488,12441]},
     {12496,[12495,12441]},
     {12499,[12498,12441]},
     {12502,[12501,12441]},
     {12505,[12504,12441]},
     {12508,[12507,12441]},
     {12535,[12527,12441]},
     {12536,[12528,12441]},
     {12537,[12529,12441]},
     {12538,[12530,12441]},
     {12542,[12541,12441]},
     {12401,[12399,12442]},
     {12404,[12402,12442]},
     {12407,[12405,12442]},
     {12410,[12408,12442]},
     {12413,[12411,12442]},
     {12497,[12495,12442]},
     {12500,[12498,12442]},
     {12503,[12501,12442]},
     {12506,[12504,12442]},
     {12509,[12507,12442]}}.
