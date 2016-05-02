%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-module(reltool_utils).

%% Public
-export([root_dir/0, erl_libs/0, lib_dirs/1,
	 split_app_name/1, prim_consult/1,
	 default_rels/0, choose_default/3,
	 normalize_dir/1,

	 assign_image_list/1, get_latest_resize/1, wait_for_stop_motion/2,
	 mod_conds/0, list_to_mod_cond/1, mod_cond_to_index/1,
	 incl_conds/0, list_to_incl_cond/1, incl_cond_to_index/1, elem_to_index/2,
	 app_dir_test/2, split_app_dir/1,
	 get_item/1, get_items/1, get_selected_items/3,
	 select_items/3, select_item/2,
	 get_column_width/1,

	 safe_keysearch/5, print/4, add_warning/3,

	 create_dir/1, list_dir/1, read_file_info/1,
	 write_file_info/2, read_file/1, write_file/2,
	 recursive_delete/1, delete/2, recursive_copy_file/2, copy_file/2,

	 throw_error/2,

	 decode_regexps/3,
	 default_val/2,
	 escript_foldl/3,

	 call/2, cast/2, reply/3]).

-include_lib("kernel/include/file.hrl").
-include_lib("wx/include/wx.hrl").
-include("reltool.hrl").

root_dir() ->
    code:root_dir().

erl_libs() ->
    string:tokens(os:getenv("ERL_LIBS", ""), ":;").

lib_dirs(Dir) ->
    case erl_prim_loader:list_dir(Dir) of
        {ok, Files} ->
	    [F || F <- Files,
		  filelib:is_dir(filename:join([Dir, F]),
				 erl_prim_loader)];
	error ->
	    []
    end.

%% "asn1-1.6.2" -> {"asn1", "1.6.2"}; "asn1" -> {"asn1", ""}
split_app_name(Name) ->
    Pred =
	fun(Elem) ->
		if
		    Elem =:= $\. -> true;
                    Elem >= $0, Elem =< $9 -> true;
                    true -> false
                end
        end,
    case lists:splitwith(Pred, lists:reverse(Name)) of
	{Vsn, [$- | App]} ->
	    {list_to_atom(lists:reverse(App)), lists:reverse(Vsn)};
	_ ->
	    {list_to_atom(Name), ""}
    end.


normalize_dir(RelDir) ->
    Tokens = filename:split(filename:absname(RelDir)),
    filename:join(lists:reverse(normalize_dir(Tokens, []))).

normalize_dir([".."|Dirs], [_Dir|Path]) ->
    normalize_dir(Dirs, Path);
normalize_dir(["."|Dirs], Path) ->
    normalize_dir(Dirs, Path);
normalize_dir([Dir|Dirs], Path) ->
    normalize_dir(Dirs, [Dir|Path]);
normalize_dir([], Path) ->
    Path.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prim_consult(Bin) when is_binary(Bin) ->
    case erl_scan:string(unicode:characters_to_list(Bin,encoding(Bin))) of
	{ok, Tokens, _EndLine} ->
	    prim_parse(Tokens, []);
	{error, {_ErrorLine, Module, Reason}, _EndLine} ->
	    {error, Module:format_error(Reason)}
    end;
prim_consult(FullName) when is_list(FullName) ->
    case erl_prim_loader:get_file(FullName) of
        {ok, Bin, _} ->
	    prim_consult(Bin);
        error ->
            {error, file:format_error(enoent)}
    end.

encoding(Bin) when is_binary(Bin) ->
    case epp:read_encoding_from_binary(Bin) of
	none ->
	    epp:default_encoding();
	E ->
	    E
    end.

prim_parse(Tokens, Acc) ->
    case lists:splitwith(fun(T) -> element(1,T) =/= dot end, Tokens) of
        {[], []} ->
            {ok, lists:reverse(Acc)};
        {Tokens2, [{dot,_} = Dot | Rest]} ->
            case erl_parse:parse_term(Tokens2 ++ [Dot]) of
                {ok, Term} ->
                    prim_parse(Rest, [Term | Acc]);
		{error, {_ErrorLine, Module, Reason}} ->
		    {error, Module:format_error(Reason)}
            end;
        {Tokens2, []} ->
            case erl_parse:parse_term(Tokens2) of
                {ok, Term} ->
                    {ok, lists:reverse([Term | Acc])};
		{error, {_ErrorLine, Module, Reason}} ->
		    {error, Module:format_error(Reason)}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default_rels() ->
    %% kernel and stdlib are added automatically in every release
    [
     #rel{name = ?DEFAULT_REL_NAME,
	  vsn = "1.0",
	  rel_apps = []},
     #rel{name = "start_sasl",
	  vsn = "1.0",
	  rel_apps = [#rel_app{name = sasl}]}
    ].

choose_default(Tag, Profile, InclDefs)
  when Profile =:= ?DEFAULT_PROFILE; InclDefs ->
    case Tag of
	incl_sys_filters  -> ?DEFAULT_INCL_SYS_FILTERS;
	excl_sys_filters  -> ?DEFAULT_EXCL_SYS_FILTERS;
	incl_app_filters  -> ?DEFAULT_INCL_APP_FILTERS;
	excl_app_filters  -> ?DEFAULT_EXCL_APP_FILTERS;
	embedded_app_type -> ?DEFAULT_EMBEDDED_APP_TYPE
    end;
choose_default(Tag, standalone, _InclDefs) ->
    case Tag of
	incl_sys_filters  -> ?STANDALONE_INCL_SYS_FILTERS;
	excl_sys_filters  -> ?STANDALONE_EXCL_SYS_FILTERS;
	incl_app_filters  -> ?STANDALONE_INCL_APP_FILTERS;
	excl_app_filters  -> ?STANDALONE_EXCL_APP_FILTERS;
	embedded_app_type -> ?DEFAULT_EMBEDDED_APP_TYPE
    end;
choose_default(Tag, embedded, _InclDefs) ->
    case Tag of
	incl_sys_filters  -> ?EMBEDDED_INCL_SYS_FILTERS;
	excl_sys_filters  -> ?EMBEDDED_EXCL_SYS_FILTERS;
	incl_app_filters  -> ?EMBEDDED_INCL_APP_FILTERS;
	excl_app_filters  -> ?EMBEDDED_EXCL_APP_FILTERS;
	embedded_app_type -> ?EMBEDDED_APP_TYPE
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assign_image_list(ListCtrl) ->
    Art = wxImageList:new(16,16),
    [wxImageList:add(Art, wxArtProvider:getBitmap(Image, [{size, {16,16}}]))
     || Image <- ["wxART_ERROR",
		  "wxART_WARNING",
                  "wxART_QUESTION",
                  "wxART_TICK_MARK",
		  "wxART_CROSS_MARK",
		  "wxART_GO_HOME"]],
    wxListCtrl:assignImageList(ListCtrl, Art, ?wxIMAGE_LIST_SMALL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_latest_resize(#wx{obj = ObjRef, event = #wxSize{}} = Wx) ->
    receive
	#wx{obj = ObjRef, event = #wxSize{}} = Wx2 ->
	    get_latest_resize(Wx2)
    after 10 ->
	    Wx
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wait_for_stop_motion(ObjRef, {_,_}=Pos) ->
    receive
	#wx{obj = ObjRef, event = #wxMouse{type = motion, x=X, y=Y}} ->
	    wait_for_stop_motion(ObjRef, {X,Y})
    after 100 ->
	    Pos
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mod_conds() ->
    ["all (ebin + app file)", "ebin + derived", "app file + derived", "derived", "none"].

list_to_mod_cond(List) ->
    case List of
	"all" ++ _   -> all;
 	"ebin" ++ _  -> ebin;
	"app" ++ _   -> app;
	"derived"    -> derived;
	"none"       -> none
    end.

mod_cond_to_index(ModCond) ->
    case ModCond of
	all       -> 0;
	ebin      -> 1;	
	app       -> 2;
	derived   -> 3;
	undefined -> 3;
	none      -> 4
    end.

incl_conds() ->
    ["include", "exclude", "derived"].

list_to_incl_cond(List) ->
    case List of
	"include" -> include;
 	"exclude" -> exclude;
	"derived" -> derived
    end.

incl_cond_to_index(ModCond) ->
    case ModCond of
	include -> 0;
	exclude -> 1;	
	derived -> 2
    end.

elem_to_index(Elem, List) ->
    elem_to_index(Elem, List, 1).

elem_to_index(Elem, [H | T], Index) ->
    case Elem =:= H of
	true -> Index;
	false -> elem_to_index(Elem, T, Index + 1)
    end;
elem_to_index(Elem, [], _) ->
    erlang:error({not_found, Elem}).

app_dir_test(Dir1, Dir2) ->
    {Name1, Vsn1, Parent1} = split_app_dir(Dir1),
    {Name2, Vsn2, Parent2} = split_app_dir(Dir2),
    if
	Name1 < Name2 -> true;
	Name1 > Name2 -> false;
	Vsn1 < Vsn2 -> false;
	Vsn1 > Vsn2 -> true;
	Parent1 =< Parent2 -> true;
	true -> false
    end.

split_app_dir(Dir) ->
    ParentDir = filename:dirname(Dir),
    Base = filename:basename(Dir),
    {Name, Vsn} = split_app_name(Base),
    Vsn2 =
	try
	    [list_to_integer(N) || N <- string:tokens(Vsn, ".")]
	catch
	    _:_ ->
		Vsn
	end,
    {Name, Vsn2, ParentDir}.

get_item(ListCtrl) ->
    case wxListCtrl:getItemCount(ListCtrl) of
	0 ->
	    undefined;
	_ ->
	    case wxListCtrl:getNextItem(ListCtrl,
					-1,
					[{geometry, ?wxLIST_NEXT_ALL},
					 {state, ?wxLIST_STATE_SELECTED}]) of
		-1 ->
		    ItemNo = wxListCtrl:getTopItem(ListCtrl),
		    case wxListCtrl:getItemText(ListCtrl, ItemNo) of
			"" ->
			    undefined;
			Text ->
			    {ItemNo, Text}
		    end;
		ItemNo ->
		    Text = wxListCtrl:getItemText(ListCtrl, ItemNo),
		    {ItemNo, Text}
	    end
    end.

get_items(ListCtrl) ->
    case wxListCtrl:getItemCount(ListCtrl) of
	0 ->
	    [];
	Count ->
	    case get_selected_items(ListCtrl, -1, []) of
		[] ->
		    ItemNo = wxListCtrl:getTopItem(ListCtrl),
		    case wxListCtrl:getItemText(ListCtrl, ItemNo) of
			"" ->
			    [];
			Text when Text =/= ?MISSING_APP_TEXT ->
			    [{ItemNo, Text}];
			_MissingText when Count > 1 ->
			    case wxListCtrl:getItemText(ListCtrl, ItemNo + 1) of
				"" ->
				    [];
				Text ->
				    [{ItemNo, Text}]
			    end;
			_MissingText ->
			    []
		    end;
		Items ->
		    Items
	    end
    end.

get_selected_items(ListCtrl, PrevItem, Acc) ->
    case wxListCtrl:getNextItem(ListCtrl,
                                PrevItem,
                                [{geometry, ?wxLIST_NEXT_ALL},
                                 {state, ?wxLIST_STATE_SELECTED}]) of
        -1 ->
	    Acc;
        ItemNo ->
	    case wxListCtrl:getItemText(ListCtrl, ItemNo) of
		Text when Text =/= ?MISSING_APP_TEXT ->
		    get_selected_items(ListCtrl,
				       ItemNo,
				       [{ItemNo, Text} | Acc]);
		_Text ->
		    get_selected_items(ListCtrl, ItemNo, Acc)
	    end
    end.

select_items(_ListCtrl, _OldItems, []) ->
    %% No new items. Nothing to select.
    false;
select_items(ListCtrl, [], Items) ->
    %% No old selection. Select first.
    select_item(ListCtrl, Items);
select_items(ListCtrl, _OldItems, [Item]) ->
    %% Only one new item. Select it.
    select_item(ListCtrl, [Item]);
select_items(ListCtrl, OldItems, NewItems) ->
    %% Try to propagate old selection to new items.
    Filter =
	fun({_OldItemNo, Text}) ->
		case lists:keysearch(Text, 2, NewItems) of
		    {value, Item} -> {true, Item};
		    false -> false
		end
	end,
    case lists:zf(Filter, OldItems) of
	[] ->
	    %% None of the old selections are valid. Select the first.
	    select_item(ListCtrl, NewItems);
	ValidItems ->
	    %% Some old selections are still valid. Select them again.
	    lists:foreach(fun(Item) -> select_item(ListCtrl, [Item]) end,
			  ValidItems)
    end.

select_item(ListCtrl, [{ItemNo, Text} | Items]) ->
    case Text =:= ?MISSING_APP_TEXT of
	true ->
	    select_item(ListCtrl, Items);
	false ->
	    StateMask = ?wxLIST_STATE_SELECTED,
	    State = wxListCtrl:getItemState(ListCtrl, ItemNo, StateMask),
	    State2 = State bor ?wxLIST_STATE_SELECTED,
	    wxListCtrl:setItemState(ListCtrl, ItemNo, State2, StateMask),
	    wxListCtrl:refreshItem(ListCtrl, ItemNo)
    end;
select_item(_ListCtrl, []) ->
    ok.

get_column_width(ListCtrl) ->
    wx:batch(fun() ->
		     {Total, _} = wxWindow:getClientSize(ListCtrl),
		     Total - scroll_size(ListCtrl)
	     end).

scroll_size(ObjRef) ->
    case os:type() of
	{win32, nt} -> 0;
	{unix, darwin} ->
	    %% I can't figure out is there is a visible scrollbar
	    %% Always make room for it
	    wxSystemSettings:getMetric(?wxSYS_VSCROLL_X);
	_ ->
	    case wxWindow:hasScrollbar(ObjRef, ?wxVERTICAL) of
		true -> wxSystemSettings:getMetric(?wxSYS_VSCROLL_X);
		false -> 0
	    end
    end.

safe_keysearch(Key, Pos, List, Mod, Line) ->
    case lists:keysearch(Key, Pos, List) of
        false ->
            io:format("~w(~w): lists:keysearch(~p, ~w, ~p) -> false\n",
                      [Mod, Line, Key, Pos, List]),
            erlang:error({Mod, Line, lists, keysearch, [Key, Pos, List]});
        {value, Val} ->
            Val
    end.

print(X, X, Format, Args) ->
    io:format(Format, Args);
print(_, _, _, _) ->
    ok.

add_warning(Format, Args, {ok,Warnings}) ->
    Warning = lists:flatten(io_lib:format(Format,Args)),
    case lists:member(Warning,Warnings) of
	true ->
	    {ok,Warnings};
	false ->
	    {ok,[Warning|Warnings]}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_dir(Dir) ->
    filelib:ensure_dir(Dir),
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        {error, Reason} ->
            Text = file:format_error(Reason),
            throw_error("create dir ~ts: ~ts", [Dir, Text])
    end.

list_dir(Dir) ->
    case erl_prim_loader:list_dir(Dir) of
        {ok, Files} ->
	    Files;
        error ->
            Text = file:format_error(enoent),
            throw_error("list dir ~ts: ~ts", [Dir, Text])
    end.

read_file_info(File) ->
    case file:read_file_info(File) of
        {ok, Info} ->
	    Info;
        {error, Reason} ->
            Text = file:format_error(Reason),
            throw_error("read file info ~ts: ~ts", [File, Text])
    end.

write_file_info(File, Info) ->
    case file:write_file_info(File, Info) of
        ok ->
	    ok;
        {error, Reason} ->
            Text = file:format_error(Reason),
            throw_error("write file info ~ts: ~ts", [File, Text])
    end.

read_file(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
	    Bin;
        {error, Reason} ->
            Text = file:format_error(Reason),
            throw_error("read file ~ts: ~ts", [File, Text])
    end.

write_file(File, IoList) ->
    case file:write_file(File, IoList) of
        ok ->
	    ok;
        {error, Reason} ->
            Text = file:format_error(Reason),
            throw_error("write file ~ts: ~ts", [File, Text])
    end.

recursive_delete(Dir) ->
    case filelib:is_dir(Dir) of
	true ->
	    case file:list_dir(Dir) of
		{ok, Files} ->
		    Fun =
			fun(F) -> recursive_delete(filename:join([Dir, F])) end,
		    lists:foreach(Fun, Files),
		    delete(Dir, directory);
		{error, enoent} ->
		    ok;
		{error, Reason} ->
		    Text = file:format_error(Reason),
		    throw_error("delete file ~ts: ~ts\n", [Dir, Text])
	    end;
	false ->
            delete(Dir, regular)
    end.

delete(File, Type) ->
    case do_delete(File, Type) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        {error, Reason} ->
            Text = file:format_error(Reason),
            throw_error("delete file ~ts: ~ts\n", [File, Text])
    end.

do_delete(File, regular) ->
    file:delete(File);
do_delete(Dir, directory) ->
    file:del_dir(Dir).

recursive_copy_file(From, To) ->
    case erl_prim_loader:list_dir(From) of
        {ok, Files} ->
	    %% Copy all files in the directory
            create_dir(To),
            Copy =
                fun(F) ->
                        recursive_copy_file(filename:join([From, F]),
					    filename:join([To, F]))
                end,
            lists:foreach(Copy, Files);
        error ->
            %% Copy single file
	    copy_file(From, To)
    end.

copy_file(From, To) ->
    case erl_prim_loader:get_file(From) of
	{ok, Bin, _} ->
	    case file:write_file(To, Bin) of
		ok ->
		    FromInfo = read_file_info(From),
		    ToInfo = read_file_info(To),
		    FromMode = FromInfo#file_info.mode,
		    ToMode = ToInfo#file_info.mode,
		    ToMode2 = FromMode bor ToMode,
		    FileInfo = ToInfo#file_info{mode = ToMode2},
		    write_file_info(To, FileInfo),
		    ok;
		{error, Reason} ->
		    Text = file:format_error(Reason),
		    throw_error("copy file ~ts -> ~ts: ~ts\n", [From, To, Text])
	    end;
	error ->
	    Text = file:format_error(enoent),
	    throw_error("copy file ~ts -> ~ts: ~ts\n", [From, To, Text])
    end.

throw_error(Format, Args) ->
    throw({error, lists:flatten(io_lib:format(Format, Args))}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_regexps(Key, Regexps, undefined) ->
    decode_regexps(Key, Regexps, []);
decode_regexps(Key, {add, Regexps}, Old) when is_list(Regexps) ->
    do_decode_regexps(Key, Regexps, Old);
decode_regexps(_Key, {del, Regexps}, Old)  when is_list(Regexps) ->
    [Re || Re <- Old, not lists:member(Re#regexp.source, Regexps)];
decode_regexps(Key, Regexps, _Old) when is_list(Regexps) ->
    do_decode_regexps(Key, Regexps, []).

do_decode_regexps(Key, [Regexp | Regexps], Acc) ->
    case catch re:compile(Regexp, [unicode]) of
        {ok, MP} ->
            do_decode_regexps(Key,
			      Regexps,
			      [#regexp{source = Regexp, compiled = MP} | Acc]);
        _ ->
            Text = lists:flatten(io_lib:format("~p", [{Key, Regexp}])),
            throw({error, "Illegal option: " ++ Text})
    end;
do_decode_regexps(_Key, [], Acc) ->
    lists:sort(Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default_val(Val, Default) ->
    case Val of
        undefined -> Default;
        _         -> Val
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escript_foldl(Fun, Acc, File) ->
    case escript:extract(File, [compile_source]) of
	{ok, [_Shebang, _Comment, _EmuArgs, Body]} ->
	    case Body of
		{source, BeamCode} ->
		    GetInfo = fun() -> file:read_file_info(File) end,
		    GetBin = fun() -> BeamCode end,
		    {ok, Fun(".", GetInfo, GetBin, Acc)};
		{beam, BeamCode} ->
		    GetInfo = fun() -> file:read_file_info(File) end,
		    GetBin = fun() -> BeamCode end,
		    {ok, Fun(".", GetInfo, GetBin, Acc)};
		{archive, ArchiveBin} ->
		    zip:foldl(Fun, Acc, {File, ArchiveBin})
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call(Name, Msg) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    {error, {noproc, Name}};
	Pid ->
	    call(Pid, Msg)
    end;
call(Pid, Msg) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {call, self(), Ref, Msg},
    receive
        {Ref, Reply} ->
            Reply;
        {'EXIT', Pid, Reason} ->
            erlang:demonitor(Ref, [flush]),
            {error, Reason};
	{'DOWN', Ref, _, _, Reason} ->
            {error, Reason}
    end.

cast(Pid, Msg) ->
    Pid ! {cast, self(), Msg},
    ok.

reply(Pid, Ref, Msg) ->
    Pid ! {Ref, Msg}.
