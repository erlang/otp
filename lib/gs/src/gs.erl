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
%% Erlang Graphics Interface and front end server
%% ------------------------------------------------------------
%%

-module(gs).
-deprecated(module).
-compile([{nowarn_deprecated_function,{gs,create,3}},
          {nowarn_deprecated_function,{gs,create,4}},
          {nowarn_deprecated_function,{gs,create_tree,2}},
          {nowarn_deprecated_function,{gs,foreach,3}},
          {nowarn_deprecated_function,{gs,read,2}},
          {nowarn_deprecated_function,{gs,start,1}}]).

%% ----- Exports -----
-export([start/0, stop/0, start/1]).
-export([create/3, create/4, is_id/1]).
-export([info/1,create_tree/2]).
-export([config/2, read/2, destroy/1]).
-export([get_id/1]).

%% ----- Not standard but convenient -----
-export([error/2,creation_error/2,assq/2,pair/2,val/2,val/3,foreach/3]).
-export([create/2]).
-export([window/1,window/2,window/3,button/1,button/2,button/3]).
-export([radiobutton/1,radiobutton/2,radiobutton/3]).
-export([checkbutton/1,checkbutton/2,checkbutton/3]).
-export([frame/1,frame/2,frame/3,label/1,label/2,label/3]).
-export([message/1,message/2,message/3]).
-export([listbox/1,listbox/2,listbox/3,entry/1,entry/2,entry/3]).
-export([scrollbar/1,scrollbar/2,scrollbar/3]).
-export([scale/1,scale/2,scale/3]).
-export([canvas/1,canvas/2,canvas/3,editor/1,editor/2,editor/3]).
-export([prompter/1,prompter/2,prompter/3]).
-export([line/1,line/2,line/3,oval/1,oval/2,oval/3]).
-export([rectangle/1,rectangle/2,rectangle/3]).
-export([polygon/1,polygon/2,polygon/3]).
-export([text/1,text/2,text/3,image/1,image/2,image/3,arc/1,arc/2,arc/3]).
-export([menu/1,menu/2,menu/3,menubutton/1,menubutton/2,menubutton/3]).
-export([menubar/1,menubar/2,menubar/3]).
-export([grid/1,grid/2,grid/3]).
-export([gridline/1,gridline/2,gridline/3]).
-export([menuitem/1,menuitem/2,menuitem/3]).

-include("gstk.hrl").

%% ----- Start/Stop -----

start() ->
    start([]).

start(Opts) ->
    Opts2 = gstk_generic:merge_default_options(gs_widgets:default_options(gs),
					      lists:sort(Opts)),
    gs_frontend:start(Opts2).

stop() ->
    gs_frontend:stop().

%% ----- Widget Commands -----

create(Objtype, Parent) ->
    GsPid = frontend(Parent),
    tag_if_ok(gs_frontend:create(GsPid,{Objtype, undefined, obj_id(Parent),[]})
	      ,GsPid).

create(Objtype, Parent, Opts) when is_list(Opts) ->
    GsPid = frontend(Parent),
    tag_if_ok(gs_frontend:create(GsPid,{Objtype,undefined,obj_id(Parent),Opts}),
	      GsPid);
create(Objtype, Parent, Opt) ->
    GsPid = frontend(Parent),
    tag_if_ok(gs_frontend:create(GsPid,
				 {Objtype,undefined,obj_id(Parent),[Opt]}),
	      GsPid).

create(Objtype, Name, Parent, Opts) when is_list(Opts) -> 
    GsPid = frontend(Parent),
    tag_if_ok(gs_frontend:create(GsPid,{Objtype, Name, obj_id(Parent),Opts}),
	      GsPid);
create(Objtype, Name, Parent, Opt) -> 
    GsPid = frontend(Parent),
    tag_if_ok(gs_frontend:create(GsPid,{Objtype,Name,obj_id(Parent),[Opt]}),
	      GsPid).

tag_if_ok(Int,Pid) when is_integer(Int) ->
    {Int,Pid};
tag_if_ok(Err,_) ->
    Err.

config(IdOrName, Options) when is_list(Options) ->
    gs_frontend:config(frontend(IdOrName),{obj_id(IdOrName),Options});
config(IdOrName, Option) ->
    gs_frontend:config(frontend(IdOrName),{obj_id(IdOrName),[Option]}).

read(IdOrName, Option) -> 
    gs_frontend:read(frontend(IdOrName),{obj_id(IdOrName),Option}).

destroy(IdOrName) -> 
    gs_frontend:destroy(frontend(IdOrName),obj_id(IdOrName)).

get_id(Name) -> 
    read(Name,id).

info(version) -> "1.3.2";
info(Option) ->
    gs_frontend:info(Option).

is_id({Int,Pid}) when is_integer(Int), is_pid(Pid) -> true;
is_id(_) -> false.

frontend({_,Pid}) when is_pid(Pid) -> Pid;
frontend({AtomName,Node}) when is_atom(AtomName),is_atom(Node) ->
    rpc:call(Node,erlang,whereis,[gs_frontend]);
frontend(Atom) when is_atom(Atom) -> whereis(gs_frontend).

obj_id({Id,_}) -> Id;
obj_id(Atom) when is_atom(Atom) -> Atom.

error(Format, Data) ->
    io:format("gs error: "),
    ok = io:format(Format, Data), % don't be quiet when Format is malformed
    io:format("~n").

creation_error(#gstkid{objtype=Ot}, {bad_result, BadResult}) ->
    {error, {creation_error,Ot,BadResult}};
creation_error(#gstkid{objtype=Ot}, BadResult) ->
    {error, {creation_error,Ot,BadResult}}.


create_tree(ParentId,[{Type,Name,Options,Children}|R]) ->
    case create(Type,Name,ParentId,Options) of
	{error,_Reason} -> {error,{create_tree,aborted_at,Type,Name}};
	Id ->
	    case create_tree(Id,Children) of
		ok -> create_tree(ParentId,R);
		Err -> Err
	    end
    end;
create_tree(ParentId,[{Type,Name,Options}|R]) when is_atom(Name) ->
    create_tree(ParentId,[{Type,Name,Options,[]}|R]);
create_tree(ParentId,[{Type,Options,Children}|R]) ->
    case create(Type,ParentId,Options) of
	{error,_Reason} -> {error,{create_tree,aborted_at,Type,Options}};
	Id ->
	    case create_tree(Id,Children) of
		ok -> create_tree(ParentId,R);
		Err -> Err
	    end
    end;
create_tree(ParentId,[{Type,Options}|R]) ->
    create_tree(ParentId,[{Type,Options,[]}|R]);
create_tree(ParentId,Tuple) when is_tuple(Tuple) ->
    create_tree(ParentId,[Tuple]);
create_tree(_,[]) ->
    ok.


window(ParentId) -> 
    create(window,ParentId,[]).
window(ParentId,Options) -> 
    create(window,ParentId,Options).
window(Name,ParentId,Options) -> 
    create(window,Name,ParentId,Options).

button(ParentId) ->
    create(button,ParentId,[]).
button(ParentId,Options) -> 
    create(button,ParentId,Options).
button(Name,ParentId,Options) -> 
    create(button,Name,ParentId,Options).

checkbutton(ParentId) ->
    create(checkbutton,ParentId,[]).
checkbutton(ParentId,Options) -> 
    create(checkbutton,ParentId,Options).

checkbutton(Name,ParentId,Options) -> 
    create(checkbutton,Name,ParentId,Options).

radiobutton(ParentId) ->
    create(radiobutton,ParentId,[]).
radiobutton(ParentId,Options) -> 
    create(radiobutton,ParentId,Options).
radiobutton(Name,ParentId,Options) -> 
    create(radiobutton,Name,ParentId,Options).

frame(ParentId) -> 
    create(frame,ParentId,[]).
frame(ParentId,Options) ->
    create(frame,ParentId,Options).
frame(Name,ParentId,Options) ->
    create(frame,Name,ParentId,Options).

canvas(ParentId) ->
    create(canvas,ParentId,[]).
canvas(ParentId,Options) ->
    create(canvas,ParentId,Options).
canvas(Name,ParentId,Options) ->
    create(canvas,Name,ParentId,Options).

label(ParentId) ->
    create(label,ParentId,[]).
label(ParentId,Options) ->
    create(label,ParentId,Options).
label(Name,ParentId,Options) ->
    create(label,Name,ParentId,Options).

message(ParentId) ->
    create(message,ParentId,[]).
message(ParentId,Options) ->
    create(message,ParentId,Options).
message(Name,ParentId,Options) ->
    create(message,Name,ParentId,Options).

listbox(ParentId) ->
    create(listbox,ParentId,[]).
listbox(ParentId,Options) ->
    create(listbox,ParentId,Options).
listbox(Name,ParentId,Options) ->
    create(listbox,Name,ParentId,Options).

entry(ParentId) -> 
    create(entry,ParentId,[]).
entry(ParentId,Options) -> 
    create(entry,ParentId,Options).
entry(Name,ParentId,Options) -> 
    create(entry,Name,ParentId,Options).

scrollbar(ParentId) -> 
    create(scrollbar,ParentId,[]).
scrollbar(ParentId,Options) ->
    create(scrollbar,ParentId,Options).
scrollbar(Name,ParentId,Options) -> 
    create(scrollbar,Name,ParentId,Options).

scale(ParentId) -> 
    create(scale,ParentId,[]).
scale(ParentId,Options) -> 
    create(scale,ParentId,Options).
scale(Name,ParentId,Options) -> 
    create(scale,Name,ParentId,Options).

editor(ParentId) ->
    create(editor,ParentId,[]).
editor(ParentId,Options) ->
    create(editor,ParentId,Options).
editor(Name,ParentId,Options) ->
    create(editor,Name,ParentId,Options).

prompter(ParentId) -> 
    create(prompter,ParentId,[]).
prompter(ParentId,Options) ->
    create(prompter,ParentId,Options).
prompter(Name,ParentId,Options) -> 
    create(prompter,Name,ParentId,Options).

line(ParentId) -> 
    create(line,ParentId,[]).
line(ParentId,Options) ->
    create(line,ParentId,Options).
line(Name,ParentId,Options) -> 
    create(line,Name,ParentId,Options).

oval(ParentId) -> 
    create(oval,ParentId,[]).
oval(ParentId,Options) -> 
    create(oval,ParentId,Options).
oval(Name,ParentId,Options) ->
    create(oval,Name,ParentId,Options).

rectangle(ParentId) ->
    create(rectangle,ParentId,[]).
rectangle(ParentId,Options) -> 
    create(rectangle,ParentId,Options).
rectangle(Name,ParentId,Options) -> 
    create(rectangle,Name,ParentId,Options).

polygon(ParentId) ->
    create(polygon,ParentId,[]).
polygon(ParentId,Options) -> 
    create(polygon,ParentId,Options).
polygon(Name,ParentId,Options) -> 
    create(polygon,Name,ParentId,Options).

text(ParentId) ->
    create(text,ParentId,[]).
text(ParentId,Options) -> 
    create(text,ParentId,Options).
text(Name,ParentId,Options) -> 
    create(text,Name,ParentId,Options).

image(ParentId) ->
    create(image,ParentId,[]).
image(ParentId,Options) -> 
    create(image,ParentId,Options).
image(Name,ParentId,Options) -> 
    create(image,Name,ParentId,Options).

arc(ParentId) ->
    create(arc,ParentId,[]).
arc(ParentId,Options) -> 
    create(arc,ParentId,Options).
arc(Name,ParentId,Options) -> 
    create(arc,Name,ParentId,Options).

menu(ParentId) ->
    create(menu,ParentId,[]).
menu(ParentId, Options) ->
    create(menu,ParentId,Options).
menu(Name,ParentId,Options) ->
    create(menu,Name,ParentId,Options).

menubutton(ParentId) ->
    create(menubutton,ParentId,[]).
menubutton(ParentId,Options) -> 
    create(menubutton,ParentId,Options).
menubutton(Name,ParentId,Options) -> 
    create(menubutton,Name,ParentId,Options).

menubar(ParentId) -> 
    create(menubar,ParentId,[]).
menubar(ParentId,Options) -> 
    create(menubar,ParentId,Options).
menubar(Name,ParentId,Options) -> 
    create(menubar,Name,ParentId,Options).

menuitem(ParentId) ->
    create(menuitem,ParentId,[]).
menuitem(ParentId,Options) ->
    create(menuitem,ParentId,Options).
menuitem(Name,ParentId,Options) ->
    create(menuitem,Name,ParentId,Options).

grid(ParentId) ->
    create(grid,ParentId,[]).
grid(ParentId,Options) ->
    create(grid,ParentId,Options).
grid(Name,ParentId,Options) ->
    create(grid,Name,ParentId,Options).

gridline(ParentId) ->
    create(gridline,ParentId,[]).
gridline(ParentId,Options) ->
    create(gridline,ParentId,Options).
gridline(Name,ParentId,Options) ->
    create(gridline,Name,ParentId,Options).

%%----------------------------------------------------------------------
%% Waiting for erl44
%%----------------------------------------------------------------------
foreach(F, ExtraArgs, [H | T]) ->
    apply(F, [H | ExtraArgs]),
    foreach(F, ExtraArgs, T);
foreach(_F, _ExtraArgs, []) -> ok.

%%----------------------------------------------------------------------
%% ASSociation with eQual key (scheme standard)
%%----------------------------------------------------------------------
assq(Key, List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {_, Val}} -> {value, Val};
	_ -> false
    end.

%%----------------------------------------------------------------------
%% When we need the whole pair.
%%----------------------------------------------------------------------
pair(Key, List) ->
    case lists:keysearch(Key, 1, List) of
	{value, Pair} -> Pair;
	_ -> false
    end.

%%----------------------------------------------------------------------
%% When we know there is a value
%%----------------------------------------------------------------------
val(Key, List) when is_list(List) ->
    {value, {_,Val}} = lists:keysearch(Key, 1, List),
    Val.

val(Key,List,ElseVal) when is_list(List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {_, Val}} -> Val;
	_ -> ElseVal
    end.

%% ----------------------------------------
%% done
