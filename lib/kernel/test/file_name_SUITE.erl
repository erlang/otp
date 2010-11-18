-module(file_name_SUITE).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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

-include("test_server.hrl").
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

-export([all/1,init_per_testcase/2, fin_per_testcase/2]).
-export([normal/1,icky/1,very_icky/1]).


init_per_testcase(_Func, Config) ->
    Dog = test_server:timetrap(test_server:seconds(60)),
    [{watchdog,Dog}|Config].

fin_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog).


all(suite) ->
    [normal,icky,very_icky].

normal(suite) ->
    [];
normal(doc) ->
    "Check file operations on normal file names regardless of unicode mode";
normal(Config) when is_list(Config) ->
    {ok,Dir} = file:get_cwd(),
    try
	Priv = ?config(priv_dir, Config),
	file:set_cwd(Priv),
	put(file_module,prim_file),
	ok = check_normal(prim_file),
	put(file_module,file),
	ok = check_normal(file)
    after
	file:set_cwd(Dir)
    end.
    

icky(suite) ->
    [];
icky(doc) ->
    "Check file operations on normal file names regardless of unicode mode";
icky(Config) when is_list(Config) ->
    {ok,Dir} = file:get_cwd(),
    try
	Priv = ?config(priv_dir, Config),
	file:set_cwd(Priv),
	put(file_module,prim_file),
	ok = check_icky(prim_file),
	put(file_module,file),
	ok = check_icky(file)
    after
	file:set_cwd(Dir)
    end.
very_icky(suite) ->
    [];
very_icky(doc) ->
    "Check file operations on normal file names regardless of unicode mode";
very_icky(Config) when is_list(Config) ->
    {ok,Dir} = file:get_cwd(),
    try
	Priv = ?config(priv_dir, Config),
	file:set_cwd(Priv),
	put(file_module,prim_file),
	case check_very_icky(prim_file) of
	    need_unicode_mode ->
		{skipped,"VM needs to be started in Unicode filename mode"};
	    ok ->
		put(file_module,file),
		ok = check_very_icky(file)
	end
    after
	file:set_cwd(Dir)
    end.
    

check_normal(Mod) -> 
    {ok,Dir} = Mod:get_cwd(),
    try
	?line make_normal_dir(Mod),
	?line {ok, L0} = Mod:list_dir("."),
	?line L1 = lists:sort(L0),
	%erlang:display(L1),
	?line L1 = lists:sort(list(normal_dir())),
	?line {ok,D2} = Mod:get_cwd(),
	?line true = is_list(D2),
	?line case Mod:altname("fil1") of
	    {error,enotsup} ->
		ok;
	    {ok,LLL} when is_list(LLL) ->
		ok
	end,
	?line [ true = is_list(El) || El <- L1],
	?line Syms = [ {S,Targ,list_to_binary(get_data(Targ,normal_dir()))} 
		 || {T,S,Targ} <- normal_dir(), T =:= symlink ],
	?line [ {ok, Cont} = Mod:read_file(SymL) || {SymL,_,Cont} <- Syms ],
	?line [ {ok, Targ} = Mod:read_link(SymL) || {SymL,Targ,_} <- Syms ],
	?line chk_cre_dir(Mod,[{directory,"temp_dir",normal_dir()}]),
	?line {ok,BeginAt} = Mod:get_cwd(),
	?line true = is_list(BeginAt),
	?line {error,enoent} = Mod:set_cwd("tmp_dir"),
	?line ok = Mod:set_cwd("temp_dir"),
	?line {ok, NowAt} = Mod:get_cwd(),
	?line true = BeginAt =/= NowAt,
	?line ok = Mod:set_cwd(".."),
	?line {ok,BeginAt} = Mod:get_cwd(),
	?line rm_r(Mod,"temp_dir"),
	?line true = is_list(Dir),
	?line [ true = is_list(FN) || FN <- L0 ],
	case has_links() of
	    true ->
		?line ok = Mod:make_link("fil1","nisse"),
		?line {ok, <<"fil1">>} = Mod:read_file("nisse"),
		?line {ok, #file_info{type = regular}} = Mod:read_link_info("nisse"),
		?line ok = Mod:delete("nisse"),
		?line {ok, <<"fil1">>} = Mod:read_file("fil1"),
		?line {error,enoent} = Mod:read_file("nisse"),
		?line {error,enoent} = Mod:read_link_info("nisse");
	    false ->
		ok
	end,
	?line [ begin
	      ?line {ok, FD} = Mod:open(Name,[read]),
	      ?line {ok, Content} = Mod:read(FD,1024),
	      ?line ok = file:close(FD)
	  end || {regular,Name,Content} <- normal_dir() ],
	?line [ begin
	      ?line {ok, FD} = Mod:open(Name,[read,binary]),
	      ?line BC = list_to_binary(Content),
	      ?line {ok, BC} = Mod:read(FD,1024),
	      ?line ok = file:close(FD)
	  end || {regular,Name,Content} <- normal_dir() ],
	?line Mod:rename("fil1","tmp_fil1"),
	?line {ok, <<"fil1">>} = Mod:read_file("tmp_fil1"),
	?line {error,enoent} = Mod:read_file("fil1"),
	?line Mod:rename("tmp_fil1","fil1"),
	?line {ok, <<"fil1">>} = Mod:read_file("fil1"),
	?line {error,enoent} = Mod:read_file("tmp_fil1"),
	?line {ok,FI} = Mod:read_file_info("fil1"),
	?line NewMode = FI#file_info.mode band (bnot 8#333),
	?line NewMode2 = NewMode bor 8#222,
	?line true = NewMode2 =/= NewMode,
	?line ok = Mod:write_file_info("fil1",FI#file_info{mode = NewMode}),
	?line {ok,#file_info{mode = NewMode}} = Mod:read_file_info("fil1"),
	?line ok = Mod:write_file_info("fil1",FI#file_info{mode = NewMode2}),
	?line {ok,#file_info{mode = NewMode2}} = Mod:read_file_info("fil1"),
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
	?line true=(length("åäö") =:= 3),
	?line UniMode = file:native_name_encoding() =/= latin1,
	?line make_icky_dir(Mod),
 	?line {ok, L0} = Mod:list_dir("."),
 	?line L1 = lists:sort(L0),
	io:format("~p ~p~n",[L1,list(icky_dir())]),
 	?line L1 = lists:sort(convlist(list(icky_dir()))),
 	?line {ok,D2} = Mod:get_cwd(),
 	?line true = is_list(D2),
%% Altname only on windows, and there are no non native filenames there
%% 	?line case Mod:altname("fil1") of
%% 	    {error,enotsup} ->
%% 		ok;
%% 	    {ok,LLL} when is_list(LLL) ->
%% 		ok
%% 	end,
 	?line [ true = ((is_list(El) or (UniMode and is_binary(El))))  || El <- L1],
 	?line Syms = [ {S,conv(Targ),list_to_binary(get_data(Targ,icky_dir()))} 
 		 || {T,S,Targ} <- icky_dir(), T =:= symlink ],
 	?line [ {ok, Cont} = Mod:read_file(SymL) || {SymL,_,Cont} <- Syms ],
 	?line [ {ok, Targ} = Mod:read_link(SymL) || {SymL,Targ,_} <- Syms ],
 	?line chk_cre_dir(Mod,[{directory,"åäö_dir",icky_dir()}]),
 	?line {ok,BeginAt} = Mod:get_cwd(),
 	?line true = is_list(BeginAt),
 	?line {error,enoent} = Mod:set_cwd("åä_dir"),
	?line ok = Mod:set_cwd("åäö_dir"),
 	?line {ok, NowAt} = Mod:get_cwd(),
 	?line true = is_list(NowAt),
 	?line true = BeginAt =/= NowAt,
 	?line ok = Mod:set_cwd(".."),
 	?line {ok,BeginAt} = Mod:get_cwd(),
 	?line rm_r2(Mod,"åäö_dir"),
	{OS,TYPE} = os:type(),
	% Check that treat_icky really converts to the same as the OS
	case UniMode of
	    true ->
		?line chk_cre_dir(Mod,[{directory,"åäö_dir",[]}]),
		?line ok = Mod:set_cwd("åäö_dir"),
		?line ok = Mod:write_file(<<"ååå">>,<<"hello">>),
		?line Treated = treat_icky(<<"ååå">>),
		?line {ok,[Treated]} = Mod:list_dir("."),
		?line ok = Mod:delete(<<"ååå">>),
		?line {ok,[]} = Mod:list_dir("."),
		?line ok = Mod:set_cwd(".."),
		?line rm_r2(Mod,"åäö_dir");
	    false ->
		ok
	end,

 	?line chk_cre_dir(Mod,[{directory,treat_icky(<<"åäö_dir">>),icky_dir()}]),
	if 
	    UniMode and (OS =/= win32) ->
		?line {error,enoent} = Mod:set_cwd("åäö_dir");
	    true ->
		ok
	end,
	?line ok = Mod:set_cwd(treat_icky(<<"åäö_dir">>)),
 	?line {ok, NowAt2} = Mod:get_cwd(),
	io:format("~p~n",[NowAt2]),
	% Cannot create raw unicode-breaking filenames on windows or macos
 	?line true = ((((not UniMode) or (OS =:= win32) or (TYPE=:=darwin)) and is_list(NowAt2)) orelse ((UniMode) and is_binary(NowAt2))),
 	?line true = BeginAt =/= NowAt2,
 	?line ok = Mod:set_cwd(".."),
 	?line {ok,BeginAt} = Mod:get_cwd(),
 	?line rm_r2(Mod,conv(treat_icky(<<"åäö_dir">>))),
	case has_links() of
	    true ->
		?line ok = Mod:make_link("fil1","nisseö"),
		?line {ok, <<"fil1">>} = Mod:read_file("nisseö"),
		?line {ok, #file_info{type = regular}} = Mod:read_link_info("nisseö"),
		?line ok = Mod:delete("nisseö"),
		?line ok = Mod:make_link("fil1",treat_icky(<<"nisseö">>)),
		?line {ok, <<"fil1">>} = Mod:read_file(treat_icky(<<"nisseö">>)),
		?line {ok, #file_info{type = regular}} = Mod:read_link_info(treat_icky(<<"nisseö">>)),
		?line ok = Mod:delete(treat_icky(<<"nisseö">>)),
		?line {ok, <<"fil1">>} = Mod:read_file("fil1"),
		?line {error,enoent} = Mod:read_file("nisseö"),
		?line {error,enoent} = Mod:read_link_info("nisseö"),
		?line {error,enoent} = Mod:read_file(treat_icky(<<"nisseö">>)),
		?line {error,enoent} = Mod:read_link_info(treat_icky(<<"nisseö">>));
	    false ->
		ok
	end,
 	?line [ begin
 	      ?line {ok, FD} = Mod:open(Name,[read]),
 	      ?line {ok, Content} = Mod:read(FD,1024),
 	      ?line ok = file:close(FD)
 	  end || {regular,Name,Content} <- icky_dir() ],
 	?line [ begin
 	      ?line {ok, FD} = Mod:open(Name,[read,binary]),
 	      ?line BC = list_to_binary([Content]),
 	      ?line {ok, BC} = Mod:read(FD,1024),
 	      ?line ok = file:close(FD)
 	  end || {regular,Name,Content} <- icky_dir() ],
 	?line Mod:rename("åäö2","åäö_fil1"),
 	?line {ok, <<"åäö2">>} = Mod:read_file("åäö_fil1"),
 	?line {error,enoent} = Mod:read_file("åäö2"),
 	?line Mod:rename("åäö_fil1","åäö2"),
 	?line {ok, <<"åäö2">>} = Mod:read_file("åäö2"),
 	?line {error,enoent} = Mod:read_file("åäö_fil1"),

 	?line Mod:rename("åäö2",treat_icky(<<"åäö_fil1">>)),
 	?line {ok, <<"åäö2">>} = Mod:read_file(treat_icky(<<"åäö_fil1">>)),
	if
	    UniMode and (OS =/= win32) ->
		{error,enoent} = Mod:read_file("åäö_fil1");
	    true ->
		ok
	end,
 	?line {error,enoent} = Mod:read_file("åäö2"),
 	?line Mod:rename(treat_icky(<<"åäö_fil1">>),"åäö2"),
 	?line {ok, <<"åäö2">>} = Mod:read_file("åäö2"),
 	?line {error,enoent} = Mod:read_file("åäö_fil1"),
 	?line {error,enoent} = Mod:read_file(treat_icky(<<"åäö_fil1">>)),

 	?line {ok,FI} = Mod:read_file_info("åäö2"),
	?line NewMode = FI#file_info.mode band (bnot 8#333),
	?line NewMode2 = NewMode bor 8#222,
 	?line true = NewMode2 =/= NewMode,
 	?line ok = Mod:write_file_info("åäö2",FI#file_info{mode = NewMode}),
 	?line {ok,#file_info{mode = NewMode}} = Mod:read_file_info("åäö2"),
 	?line ok = Mod:write_file_info("åäö2",FI#file_info{mode = NewMode2}),
 	?line {ok,#file_info{mode = NewMode2}} = Mod:read_file_info("åäö2"),

 	?line {ok,FII} = Mod:read_file_info(treat_icky(<<"åäö5">>)),
 	?line true = NewMode2 =/= NewMode,
 	?line ok = Mod:write_file_info(treat_icky(<<"åäö5">>),FII#file_info{mode = NewMode}),
 	?line {ok,#file_info{mode = NewMode}} = Mod:read_file_info(treat_icky(<<"åäö5">>)),
 	?line ok = Mod:write_file_info(<<"åäö5">>,FII#file_info{mode = NewMode2}),
 	?line {ok,#file_info{mode = NewMode2}} = Mod:read_file_info(treat_icky(<<"åäö5">>)),
	ok
    after
	Mod:set_cwd(Dir),
        io:format("Wd now: ~s~n",[Dir])
    end.

check_very_icky(Mod) -> 
    {ok,Dir} = Mod:get_cwd(),
    try
	?line true=(length("åäö") =:= 3),
	?line UniMode = file:native_name_encoding() =/= latin1,
	if
	    not UniMode ->
		throw(need_unicode_mode);
	    true ->
		ok
	end,
	?line make_very_icky_dir(Mod),
 	?line {ok, L0} = Mod:list_dir("."),
 	?line L1 = lists:sort(L0),
 	?line L1 = lists:sort(convlist(list(very_icky_dir()))),
 	?line {ok,D2} = Mod:get_cwd(),
 	?line true = is_list(D2),
 	?line [ true = ((is_list(El) or is_binary(El)))  || El <- L1],
 	?line Syms = [ {S,conv(Targ),list_to_binary(get_data(Targ,very_icky_dir()))} 
 		 || {T,S,Targ} <- very_icky_dir(), T =:= symlink ],
 	?line [ {ok, Cont} = Mod:read_file(SymL) || {SymL,_,Cont} <- Syms ],
 	?line [ {ok, Targ} = Mod:read_link(SymL) || {SymL,Targ,_} <- Syms ],
 	?line chk_cre_dir(Mod,[{directory,[1088,1079,1091]++"_dir",very_icky_dir()}]),
 	?line {ok,BeginAt} = Mod:get_cwd(),
 	?line true = is_list(BeginAt),
 	?line {error,enoent} = Mod:set_cwd("åä_dir"),
	?line ok = Mod:set_cwd([1088,1079,1091]++"_dir"),
 	?line {ok, NowAt} = Mod:get_cwd(),
 	?line true = is_list(NowAt),
 	?line true = BeginAt =/= NowAt,
 	?line ok = Mod:set_cwd(".."),
 	?line {ok,BeginAt} = Mod:get_cwd(),
 	?line rm_r2(Mod,[1088,1079,1091]++"_dir"),

	case has_links() of
	    true ->
		?line ok = Mod:make_link("fil1","nisse"++[1088,1079,1091]),
		?line {ok, <<"fil1">>} = 
		    Mod:read_file("nisse"++[1088,1079,1091]),
		?line {ok, #file_info{type = regular}} = 
		    Mod:read_link_info("nisse"++[1088,1079,1091]),
		?line ok = Mod:delete("nisse"++[1088,1079,1091]),
		?line ok = Mod:make_link("fil1",<<"nisseö">>),
		?line {ok, <<"fil1">>} = Mod:read_file(<<"nisseö">>),
		?line {ok, #file_info{type = regular}} = 
		    Mod:read_link_info(<<"nisseö">>),
		?line ok = Mod:delete(<<"nisseö">>),
		?line {ok, <<"fil1">>} = Mod:read_file("fil1"),
		?line {error,enoent} = Mod:read_file("nisse"++[1088,1079,1091]),
		?line {error,enoent} = Mod:read_link_info("nisse"++[1088,1079,1091]),
		?line {error,enoent} = Mod:read_file(<<"nisseö">>),
		?line {error,enoent} = Mod:read_link_info(<<"nisseö">>);
	    false ->
		ok
	end,
 	?line [ begin
 	      ?line {ok, FD} = Mod:open(Name,[read]),
 	      ?line {ok, Content} = Mod:read(FD,1024),
 	      ?line ok = file:close(FD)
 	  end || {regular,Name,Content} <- very_icky_dir() ],
 	?line [ begin
 	      ?line {ok, FD} = Mod:open(Name,[read,binary]),
 	      ?line BC = list_to_binary([Content]),
 	      ?line {ok, BC} = Mod:read(FD,1024),
 	      ?line ok = file:close(FD)
 	  end || {regular,Name,Content} <- very_icky_dir() ],
 	?line Mod:rename([956,965,963,954,959,49],
			 [956,965,963,954,959]++"_fil1"),
 	?line {ok, <<"åäö2">>} = Mod:read_file([956,965,963,954,959]++"_fil1"),
 	?line {error,enoent} = Mod:read_file([956,965,963,954,959,49]),
 	?line Mod:rename([956,965,963,954,959]++"_fil1",[956,965,963,954,959,49]),
 	?line {ok, <<"åäö2">>} = Mod:read_file([956,965,963,954,959,49]),
 	?line {error,enoent} = Mod:read_file([956,965,963,954,959]++"_fil1"),

 	?line {ok,FI} = Mod:read_file_info([956,965,963,954,959,49]),
	?line NewMode = FI#file_info.mode band (bnot 8#333),
	?line NewMode2 = NewMode bor 8#222,
 	?line true = NewMode2 =/= NewMode,
 	?line ok = Mod:write_file_info([956,965,963,954,959,49],
				       FI#file_info{mode = NewMode}),
 	?line {ok,#file_info{mode = NewMode}} = 
	           Mod:read_file_info([956,965,963,954,959,49]),
 	?line ok = Mod:write_file_info([956,965,963,954,959,49],
				       FI#file_info{mode = NewMode2}),
 	?line {ok,#file_info{mode = NewMode2}} = 
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
	    {ok, Content} = Mod:list_dir(Dir),
	    [ rm_rf(Mod,filename:join(Dir,C)) || C <- Content ],
	    Mod:del_dir(Dir),
	    ok;
	{ok, #file_info{}} ->
	    Mod:delete(Dir);
	_ ->
	    ok
    end.

rm_r(Mod,Dir) ->
    %erlang:display({rm_r,Dir}),
    case  Mod:read_link_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    {ok,#file_info{type = directory}} =  Mod:read_file_info(Dir),
	    {ok, Content} = Mod:list_dir(Dir),
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
    %erlang:display({rm_r2,Dir}),
    case  Mod:read_link_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    {ok,#file_info{type = directory}} =  Mod:read_file_info(Dir),
	    {ok, Content} = Mod:list_dir(Dir),
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
chk_cre_dir(_,[]) ->
    ok;
chk_cre_dir(Mod,[{regular,Name,Content}|T]) ->
    ok = Mod:write_file(Name,Content),
    chk_cre_dir(Mod,T);
chk_cre_dir(Mod,[{link,Name,Target}|T]) ->
    ok = Mod:make_link(Target,Name),
    chk_cre_dir(Mod,T);
chk_cre_dir(Mod,[{symlink,Name,Target}|T]) ->
    ok = Mod:make_symlink(Target,Name),
    chk_cre_dir(Mod,T);
chk_cre_dir(Mod,[{directory,Name,Content}|T]) ->
    ok = Mod:make_dir(Name),
    Content2 = [{Ty,filename:join(Name,N),case Ty of link -> filename:join(Name,C); _ -> C end} || {Ty,N,C} <- Content ],
    chk_cre_dir(Mod,Content2),
    chk_cre_dir(Mod,T).
 
has_links() ->   
    case os:type() of
	{win32,_} ->
	    case os:version() of
		{N,NN,_} when (N > 5) orelse (NN > 1) ->
		    true;
		_ ->
		    false
	    end;
	_ ->
	    true
    end.

make_normal_dir(Mod) ->
    rm_rf(Mod,"normal_dir"),
    Mod:make_dir("normal_dir"),
    Mod:set_cwd("normal_dir"),
    Mod:write_file("fil1","fil1"),
    Mod:write_file("fil2","fil2"),
    case has_links() of
	true ->
	    Mod:make_link("fil2","fil3"),
	    Mod:make_symlink("fil2","fil4");
	_ ->
	    ok
    end,
    Mod:make_dir("subdir"),
    Mod:write_file(filename:join("subdir","subfil1"),"subfil1"),
    ok.
    
normal_dir() ->
    [{regular,"fil1","fil1"},
     {regular,"fil2","fil2"}] ++
	case has_links() of
	    true ->
		[{regular,"fil3","fil2"},
		 {symlink,"fil4","fil2"}];
	    false ->
		[]
	end ++
	[{directory,"subdir",
	  [{regular,"subfil1","subfil1"}]}].

make_icky_dir(Mod) ->
    rm_rf(Mod,"icky_dir"),
    Icky=icky_dir(),
    chk_cre_dir(Mod,[{directory,"icky_dir",linkify([],Icky)}]),
    Mod:set_cwd("icky_dir"),
    ok.

linkify(_Passed,[]) ->
    [];
linkify(Passed,[{regular,Name,Content}|T]) ->
    Regulars = [ {N,C} || {regular,N,C} <- Passed, N =/= Name ],
    case lists:keysearch(Content,2,Regulars) of
	{value, {Linkto, Content}} ->
	    [{link,Name,Linkto} | linkify(Passed,T)];
	_ ->
	    [{regular,Name,Content} | linkify([{regular,Name,Content}|Passed],T)]
    end;
linkify(Passed,[{directory, Name, Content}|T]) ->
    [{directory,Name, linkify(Content,Content)}|linkify(Passed,T)];
linkify(Passed,[H|T]) ->
    [H|linkify([H|Passed],T)].

icky_dir() ->
    [{regular,"fil1","fil1"},
     {regular,"åäö2","åäö2"}] ++
	case has_links() of
	    true ->
		[{regular,"åäö3","åäö2"},
		 {symlink,"åäö4","åäö2"}];
	    false ->
		[]
	end ++
	[{regular,treat_icky(<<"åäö5">>),"åäö5"}] ++
	 case has_links() of
	     true -> 
		 [{symlink,treat_icky(<<"åäö6">>),treat_icky(<<"åäö5">>)}];
	     false -> 
		 []
	 end ++
	[{directory,treat_icky(<<"åäösubdir2">>),
	  [{regular,treat_icky(<<"åäösubfil2">>),"åäösubfil12"},
	   {regular,"åäösubfil3","åäösubfil13"}]},
	 {directory,"åäösubdir",
	  [{regular,"åäösubfil1","åäösubfil1"}]}].

make_very_icky_dir(Mod) ->
    rm_rf(Mod,"very_icky_dir"),
    Icky=very_icky_dir(),
    chk_cre_dir(Mod,[{directory,"very_icky_dir",linkify([],Icky)}]),
    Mod:set_cwd("very_icky_dir"),
    ok.

very_icky_dir() ->
    [{regular,"fil1","fil1"},
     {regular,[956,965,963,954,959,49],"åäö2"}] ++
	case has_links() of
	    true ->
		[{regular,[956,965,963,954,959,50],"åäö2"},
		 {symlink,[956,965,963,954,959,51],[956,965,963,954,959,49]}];
	    false ->
		[]
	end ++
     [{regular,treat_icky(<<"åäö5">>),"åäö5"}] ++
	case has_links() of
	    true ->
		[{symlink,treat_icky(<<"åäö6">>),treat_icky(<<"åäö5">>)}];
	    false -> 
		[]
	end ++
      [{directory,treat_icky(<<"åäösubdir2">>),
      [{regular,treat_icky(<<"åäösubfil2">>),"åäösubfil12"},
       {regular,"åäösubfil3","åäösubfil13"}]},
      {directory,[956,965,963,954,959]++"subdir1",
       [{regular,[956,965,963,954,959]++"subfil1","åäösubfil1"}]}].

%% Some OS'es simply do not allow non UTF8 filenames
treat_icky(Bin) ->
    case os:type() of
	{unix,darwin} ->
	    procentify(Bin);
	{win32,_} ->
	    binary_to_list(Bin);
	_ ->
	    Bin
    end.

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
    %erlang:display('Convert...'),
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

	
    

