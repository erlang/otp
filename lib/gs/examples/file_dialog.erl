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

%%
%% ------------------------------------------------------------
%% File Selection Dialog
%% ------------------------------------------------------------

-module(file_dialog).
-compile([{nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,create,4}},
          {nowarn_deprecated_function,{gs,label,2}},
          {nowarn_deprecated_function,{gs,read,2}},
          {nowarn_deprecated_function,{gs,start,0}}]).

-export([start/0,start/1,start/2,fs_init/3]).

-include_lib("kernel/include/file.hrl").


%% ----- File Selection ----
start() ->
    {ok,Dir}=file:get_cwd(),
    start(Dir,[]).

start(Dir) ->
    start(Dir,[]).

start(Dir,File) ->
    Dir0 = case lists:last(Dir) of
	$/ -> Dir;
	_  -> lists:append(Dir,"/")
    end,
    Pid=spawn(file_dialog,fs_init,[Dir0,File,self()]),	
    receive
	{file_dialog,Pid,Result} -> Result
    end.


%% ------------------------------------------------------------
fs_init(Dir,File,Owner) ->
    S=gs:start(),
    gs:create(window,win,S,[{width,250},{height,265},{title,"File Dialog"},
			    {configure,true}]),
    gs:create(label,label,win,[{y,0},{width,250},{label, {text,Dir}}]),
    gs:label(win,[{width,50},{y,30},{x,5},{label, {text,"File:"}}]),
    gs:create(entry,entry,win,[{y,30},{width,190},{x,55},
			{keypress,true},{focus,true}]),
    gs:create(listbox,lb,win,[{x,5},{y,60},{width,160},{height,199},
			   {vscroll,right},{click,true},{doubleclick,true}]),
    gs:create(button,ok,win,[{label, {text,"OK"}},{width,40},{x,185},{y,170}]),
    gs:create(button,cancel,win,[{label, {text,"Cancel"}},{x,175},{y,220},{width,65}]),
    Items=refresh(Dir),
    %% --- select File if it's given ---
    case index_member(File,Items) of
	{ok,Index} ->
	    gs:config(lb,{selection,clear}),
	    gs:config(lb,{selection,Index});
	_ -> true
    end,
    gs:config(win,{map,true}),
    fs_loop(Dir,Owner).

fs_loop(Dir,Owner) ->
    receive
	{gs,ok,click,_,_} ->
	    entered_name(Dir,Owner);
	{gs,cancel,click,_,_} ->
	    Owner ! {file_dialog,self(),cancel};
	{gs,entry,keypress,_,['Return'|_]} ->
	    entered_name(Dir,Owner);
	{gs,entry,keypress,_,[_Keysym|_]} ->
	    fs_loop(Dir,Owner);
	{gs,lb,click,_,_} ->
	    clicked(Dir,Owner);
	{gs,lb,doubleclick,_,_} ->
	    double_clicked(Dir,Owner);
	{gs,win,configure,_,[250,265|_]} -> % already got that size
	    fs_loop(Dir,Owner);
	{gs,win,configure,_,_} ->
	    gs:config(win,[{geometry,{250,265}}]),
	    fs_loop(Dir,Owner);
	stop ->
	    exit(normal);
	{gs,_,destroy,_,_} -> 
	    Owner ! {file_dialog,self(),cancel};
	X ->
	    io:format("file_dialog: got other: ~w.~n",[X]),
	    fs_loop(Dir,Owner)
    end.



refresh(Dir) ->
    gs:config(lb,clear),
    gs:config(label,{label, {text,Dir}}),
    gs:config(entry,{text,""}),
    Items=["../"|get_files(Dir)],
    gs:config(lb,[{items,Items}]),
    Items.

    
entered_name(Dir,Owner) ->
    File=gs:read(entry,text),
    case check_file(Dir,File) of
	{file,Dir2,File2} ->
	    Owner ! {file_dialog,self(),{ok,Dir2,File2}};
	{dir,Dir2} ->
	    refresh(Dir2),
	    fs_loop(Dir2,Owner);
	{error,no_file} ->
	    double_clicked(Dir,Owner);
	_ ->
	    fs_loop(Dir,Owner)
    end.


clicked(Dir,Owner) ->
    [Idx|_]=gs:read(lb,selection),
    File=gs:read(lb,{get,Idx}),
    case lists:last(File) of
	$/ -> %it's a dir
	    true;
	_ -> % it's a file
	    gs:config(entry,{text,File})
    end,
    fs_loop(Dir,Owner).


double_clicked(Dir,Owner) ->
    case gs:read(lb,selection) of
	[0] -> % up one dir
	    NewDir=up_one_dir(Dir),
	    refresh(NewDir),
	    fs_loop(NewDir,Owner);
	[] ->
	    fs_loop(Dir,Owner);
	[Idx] ->
	    File=gs:read(lb,{get,Idx}),
	    case lists:last(File) of
		$/ -> % down a dir
		    NewDir=lists:append(Dir,File),
		    refresh(NewDir),
		    fs_loop(NewDir,Owner);
		_ -> % done
		    Owner!{file_dialog,self(),{ok,Dir,File}}
	    end
    end.


%% checks if a file exists
%% returns {file,Dir,File}
%%         {dir,Dir}
%%    or   {error,What}
check_file(Dir,File) ->
    case catch lists:last(File) of
	$/ -> % File is a Dir
	    NewDir = case File of
			 [$/|_] -> %absolute path
			     File;
			 _ -> %relative path
			     lists:append(Dir,File)
	    end,
	    case file:list_dir(NewDir) of
		{ok,_} -> {dir,NewDir};
		_      -> {error,bad_dir}
	    end;
	{'EXIT',_Why} -> {error,no_file};
	_ ->
	    Words=string:tokens(File,[$/,$\\]),
	    NewFile=lists:last(Words),
	    NewDir = case File of
		[$/|_] -> %absolute path
		    up_one_dir(File);
		_ -> %relative path
		    case up_one_dir(File) of
			[$/]        -> Dir;
			[$/|SubDir] -> lists:flatten([Dir,SubDir,$/])
		    end
	    end,
	    case file:read_file_info(lists:append(NewDir,NewFile)) of
		{ok,_} -> 
		    {file,NewDir,NewFile};
		_ -> 
		    {error,bad_file}
	    end
    end.

	        
get_files(Dir) -> 
    {ok,Files} = file:list_dir(Dir),
    add_slash(Dir,lists:sort(Files)).
 
add_slash(_,[]) -> [];
add_slash(Dir,[H|T]) ->
    case file:read_file_info(lists:append(Dir,[$/|H])) of
	{ok,FI} when FI#file_info.type==directory ->
            [lists:append(H,"/")|add_slash(Dir,T)];
        _ ->
	    [H|add_slash(Dir,T)]
    end.


%filter([H|T]) ->
%    case lists:last(H) of
%	$/ -> [H|filter(T)];
%	_ ->
%	    Len =length(H),
%	    if Len>4 ->
%		    case lists:nthtail(Len-4,H) of
%			".erl" -> [H|filter(T)];
%			_ -> filter(T)
%		    end;
%		true -> filter(T)
%	    end
%    end;
%filter([]) ->    
%    [].
	
    
%% like member but also returns index
index_member(Item,List) ->
    i_m(0,Item,List).

i_m(N,Item,[Item|_List]) ->
    {ok,N};
i_m(N,Item,[_|List]) ->
    i_m(N+1,Item,List);
i_m(_N,_Item,[]) ->
    false.

up_one_dir(Dir) ->
    L =string:tokens(Dir,[$/,$\\]),
    lists:flatten(rem_last(L)).

rem_last([_Last]) ->
    [$/];
rem_last([Head|Tail]) ->
    [$/,Head|rem_last(Tail)];
rem_last([]) ->
    [$/].
		  
%% ----------------------------------------
%% done
