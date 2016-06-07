%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%%----------------------------------------------------------------------
%%% Purpose : Wrapper library for GS to provide proper error handling
%%%----------------------------------------------------------------------

-module(gse).
-compile([{nowarn_deprecated_function,{gs,arc,2}},
          {nowarn_deprecated_function,{gs,arc,3}},
          {nowarn_deprecated_function,{gs,button,2}},
          {nowarn_deprecated_function,{gs,button,3}},
          {nowarn_deprecated_function,{gs,canvas,2}},
          {nowarn_deprecated_function,{gs,canvas,3}},
          {nowarn_deprecated_function,{gs,checkbutton,2}},
          {nowarn_deprecated_function,{gs,checkbutton,3}},
          {nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,create,3}},
          {nowarn_deprecated_function,{gs,create,4}},
          {nowarn_deprecated_function,{gs,create_tree,2}},
          {nowarn_deprecated_function,{gs,destroy,1}},
          {nowarn_deprecated_function,{gs,editor,2}},
          {nowarn_deprecated_function,{gs,editor,3}},
          {nowarn_deprecated_function,{gs,entry,2}},
          {nowarn_deprecated_function,{gs,entry,3}},
          {nowarn_deprecated_function,{gs,frame,2}},
          {nowarn_deprecated_function,{gs,frame,3}},
          {nowarn_deprecated_function,{gs,grid,2}},
          {nowarn_deprecated_function,{gs,grid,3}},
          {nowarn_deprecated_function,{gs,gridline,2}},
          {nowarn_deprecated_function,{gs,gridline,3}},
          {nowarn_deprecated_function,{gs,image,2}},
          {nowarn_deprecated_function,{gs,image,3}},
          {nowarn_deprecated_function,{gs,label,2}},
          {nowarn_deprecated_function,{gs,label,3}},
          {nowarn_deprecated_function,{gs,line,2}},
          {nowarn_deprecated_function,{gs,line,3}},
          {nowarn_deprecated_function,{gs,listbox,2}},
          {nowarn_deprecated_function,{gs,listbox,3}},
          {nowarn_deprecated_function,{gs,menu,2}},
          {nowarn_deprecated_function,{gs,menu,3}},
          {nowarn_deprecated_function,{gs,menubar,2}},
          {nowarn_deprecated_function,{gs,menubar,3}},
          {nowarn_deprecated_function,{gs,menubutton,2}},
          {nowarn_deprecated_function,{gs,menubutton,3}},
          {nowarn_deprecated_function,{gs,menuitem,2}},
          {nowarn_deprecated_function,{gs,menuitem,3}},
          {nowarn_deprecated_function,{gs,message,2}},
          {nowarn_deprecated_function,{gs,message,3}},
          {nowarn_deprecated_function,{gs,oval,2}},
          {nowarn_deprecated_function,{gs,oval,3}},
          {nowarn_deprecated_function,{gs,polygon,2}},
          {nowarn_deprecated_function,{gs,polygon,3}},
          {nowarn_deprecated_function,{gs,prompter,2}},
          {nowarn_deprecated_function,{gs,prompter,3}},
          {nowarn_deprecated_function,{gs,radiobutton,2}},
          {nowarn_deprecated_function,{gs,radiobutton,3}},
          {nowarn_deprecated_function,{gs,read,2}},
          {nowarn_deprecated_function,{gs,rectangle,2}},
          {nowarn_deprecated_function,{gs,rectangle,3}},
          {nowarn_deprecated_function,{gs,scale,2}},
          {nowarn_deprecated_function,{gs,scale,3}},
          {nowarn_deprecated_function,{gs,scrollbar,2}},
          {nowarn_deprecated_function,{gs,scrollbar,3}},
          {nowarn_deprecated_function,{gs,start,0}},
          {nowarn_deprecated_function,{gs,start,1}},
          {nowarn_deprecated_function,{gs,text,2}},
          {nowarn_deprecated_function,{gs,text,3}},
          {nowarn_deprecated_function,{gs,window,2}},
          {nowarn_deprecated_function,{gs,window,3}}]).

%%-compile(export_all).
-export([
	 start/0,
	 start/1,
	 create/3,
	 create_named/4,
	 config/2,
	 read/2,
	 destroy/1,
	 create_tree/2,
	 window/2,
	 named_window/3,
	 button/2,
	 named_button/3,
	 checkbutton/2,
	 named_checkbutton/3,
	 radiobutton/2,
	 named_radiobutton/3,
	 frame/2,
	 named_frame/3,
	 canvas/2,
	 named_canvas/3,
	 label/2,
	 named_label/3,
	 message/2,
	 named_message/3,
	 listbox/2,
	 named_listbox/3,
	 entry/2,
	 named_entry/3,
	 scrollbar/2,
	 named_scrollbar/3,
	 scale/2,
	 named_scale/3,
	 editor/2,
	 named_editor/3,
	 prompter/2,
	 named_prompter/3,
	 line/2,
	 named_line/3,
	 oval/2,
	 named_oval/3,
	 rectangle/2,
	 named_rectangle/3,
	 polygon/2,
	 named_polygon/3,
	 text/2,
	 named_text/3,
	 image/2,
	 named_image/3,
	 arc/2,
	 named_arc/3,
	 menu/2,
	 named_menu/3,
	 menubutton/2,
	 named_menubutton/3,
	 menubar/2,
	 named_menubar/3,
	 menuitem/2,
	 named_menuitem/3,
	 grid/2,
	 named_grid/3,
	 gridline/2,
	 named_gridline/3,
	 %% Convenience functions
	 enable/1,
	 disable/1,
	 select/1,
	 deselect/1,
	 map/1,
	 unmap/1,
	 resize/3,
	 name_occupied/1
	 
	]).


%%
%% gse:start()
%% Returns: 
%%     An identifier to a top object for the graphic system
%%
%% Errors:
%%     Exits with a {?MODULE,start,Reason} if there is a problem
%%     creating the top level graphic object.
%%


start() ->
    case gs:start() of
	{error,Reason} ->
	    exit({?MODULE, start,Reason});
	Return -> Return
    end.

%%
%% gse:start(Opts)
%% Returns: 
%%     An identifier to a top object for the graphic system
%%
%% Errors:
%%     Exits with a {?MODULE,start,Reason} if there is a problem
%%     creating the top level graphic object.
%%


start(Opts) ->
    case gs:start(Opts) of
	{error,Reason} ->
	    exit({?MODULE, start,Reason});
	Return -> Return
    end.

%%
%% gse:create(Objtype,Parent,Opts) replaces
%% the unnecessary functions:
%%     gs:create(Obj,Parent)
%%     gs:create(Obj,Parent,Opt)
%%     gs:create(Obj,Parent)
%%     gs:create(Obj,Parent)
%%
%% Returns:
%%     An identifier for the created object
%%
%% Errors: {?MODULE, create, Reason}, where Reason is one of:
%%    {no_such_parent, Parent}
%%    {unknown_type, Type}
%%    {incvalid_option, Type, {Option,Value}}
%%
%%
create(Objtype,Parent,Opts) when is_list(Opts) ->
    case gs:create(Objtype,Parent,Opts) of
	{error,Reason} ->
	    exit({?MODULE, create,Reason});
	Return -> Return
    end.
    

%%
%% gse:create_named(Name, Objtype,Parent, Opts) replaces
%% the confusing
%%     gs:create(Name,Objtype, Parent, Opts)
%%
%% Returns:
%%     An identifier for the created object
%%
%% Errors: {?MODULE, create, Reason}, where Reason is one of:
%%    {no_such_parent, Parent}
%%    {unknown_type, Type}
%%    {incvalid_option, Type, {Option,Value}}
%%    {name_occupied,Name}
%%

create_named(Name,Objtype,Parent,Opts) when is_list(Opts) ->
    case gs:create(Objtype,Name,Parent,Opts) of
	{error,Reason} ->
	    exit({?MODULE, create_named,Reason});
	Return -> Return
    end.
    


%%
%% gse:config(Object, Options) replaces
%% the unnecessary
%%     gs:config(Object, Opt)
%%

config(Object,Opts) when is_list(Opts) ->
    case gs:config(Object,Opts) of
	{error,Reason} ->
	    exit({?MODULE, config,Reason});
	Return -> Return
    end.

%%
%% gs:read(Object, OptionKey)
%%
read(Object,OptionKey) ->
    case gs:read(Object,OptionKey) of
	{error,Reason} ->
	    exit({?MODULE, read,Reason});
	Return -> Return
    end.

%%
%% gs:destroy(Object)
%%

destroy(Object)->
    case gs:destroy(Object) of
	{error,Reason} ->
	    exit({?MODULE, destroy,Reason});
	Return -> Return
    end.

%%
%% gs:create_tree
%%

create_tree(Parent, Tree)->
    case gs:create_tree(Parent,Tree) of
	{error,Reason} ->
	    exit({?MODULE, create_tree,Reason});
	Return -> Return
    end.


window(Parent,Options) when is_list(Options) ->
    case gs:window(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,window,Reason});
	Return -> Return
    end.

named_window(Name,Parent,Options) when is_list(Options) ->
    case gs:window(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_window,Reason});
	Return -> Return
    end.

    
button(Parent,Options) when is_list(Options) ->
    case gs:button(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,button,Reason});
	Return -> Return
    end.


named_button(Name,Parent,Options) when is_list(Options) ->
    case gs:button(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_button,Reason});
	Return -> Return
    end.

    
checkbutton(Parent,Options) when is_list(Options) ->
    case gs:checkbutton(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,checkbutton,Reason});
	Return -> Return
    end.


named_checkbutton(Name,Parent,Options) when is_list(Options) ->
    case gs:checkbutton(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_checkbutton,Reason});
	Return -> Return
    end.

    
radiobutton(Parent,Options) when is_list(Options) ->
    case gs:radiobutton(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,radiobutton,Reason});
	Return -> Return
    end.


named_radiobutton(Name,Parent,Options) when is_list(Options) ->
    case gs:radiobutton(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_radiobutton,Reason});
	Return -> Return
    end.

    
frame(Parent,Options) when is_list(Options) ->
    case gs:frame(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,frame,Reason});
	Return -> Return
    end.


named_frame(Name,Parent,Options) when is_list(Options) ->
    case gs:frame(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_frame,Reason});
	Return -> Return
    end.

    
canvas(Parent,Options) when is_list(Options) ->
    case gs:canvas(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,canvas,Reason});
	Return -> Return
    end.


named_canvas(Name,Parent,Options) when is_list(Options) ->
    case gs:canvas(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_canvas,Reason});
	Return -> Return
    end.

    
label(Parent,Options) when is_list(Options) ->
    case gs:label(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,label,Reason});
	Return -> Return
    end.


named_label(Name,Parent,Options) when is_list(Options) ->
    case gs:label(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_label,Reason});
	Return -> Return
    end.

    
message(Parent,Options) when is_list(Options) ->
    case gs:message(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,message,Reason});
	Return -> Return
    end.


named_message(Name,Parent,Options) when is_list(Options) ->
    case gs:message(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_message,Reason});
	Return -> Return
    end.

    
listbox(Parent,Options) when is_list(Options) ->
    case gs:listbox(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,listbox,Reason});
	Return -> Return
    end.


named_listbox(Name,Parent,Options) when is_list(Options) ->
    case gs:listbox(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_listbox,Reason});
	Return -> Return
    end.

    
entry(Parent,Options) when is_list(Options) ->
    case gs:entry(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,entry,Reason});
	Return -> Return
    end.


named_entry(Name,Parent,Options) when is_list(Options) ->
    case gs:entry(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_entry,Reason});
	Return -> Return
    end.

    
scrollbar(Parent,Options) when is_list(Options) ->
    case gs:scrollbar(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,scrollbar,Reason});
	Return -> Return
    end.


named_scrollbar(Name,Parent,Options) when is_list(Options) ->
    case gs:scrollbar(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_scrollbar,Reason});
	Return -> Return
    end.

    
scale(Parent,Options) when is_list(Options) ->
    case gs:scale(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,scale,Reason});
	Return -> Return
    end.


named_scale(Name,Parent,Options) when is_list(Options) ->
    case gs:scale(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_scale,Reason});
	Return -> Return
    end.

    
editor(Parent,Options) when is_list(Options) ->
    case gs:editor(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,editor,Reason});
	Return -> Return
    end.


named_editor(Name,Parent,Options) when is_list(Options) ->
    case gs:editor(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_editor,Reason});
	Return -> Return
    end.

    
prompter(Parent,Options) when is_list(Options) ->
    case gs:prompter(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,prompter,Reason});
	Return -> Return
    end.


named_prompter(Name,Parent,Options) when is_list(Options) ->
    case gs:prompter(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_prompter,Reason});
	Return -> Return
    end.

    
line(Parent,Options) when is_list(Options) ->
    case gs:line(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,line,Reason});
	Return -> Return
    end.


named_line(Name,Parent,Options) when is_list(Options) ->
    case gs:line(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_line,Reason});
	Return -> Return
    end.

    
oval(Parent,Options) when is_list(Options) ->
    case gs:oval(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,oval,Reason});
	Return -> Return
    end.


named_oval(Name,Parent,Options) when is_list(Options) ->
    case gs:oval(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_oval,Reason});
	Return -> Return
    end.

    
rectangle(Parent,Options) when is_list(Options) ->
    case gs:rectangle(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,rectangle,Reason});
	Return -> Return
    end.


named_rectangle(Name,Parent,Options) when is_list(Options) ->
    case gs:rectangle(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_rectangle,Reason});
	Return -> Return
    end.

    
polygon(Parent,Options) when is_list(Options) ->
    case gs:polygon(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,polygon,Reason});
	Return -> Return
    end.


named_polygon(Name,Parent,Options) when is_list(Options) ->
    case gs:polygon(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_polygon,Reason});
	Return -> Return
    end.

    
text(Parent,Options) when is_list(Options) ->
    case gs:text(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,text,Reason});
	Return -> Return
    end.


named_text(Name,Parent,Options) when is_list(Options) ->
    case gs:text(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_text,Reason});
	Return -> Return
    end.

    
image(Parent,Options) when is_list(Options) ->
    case gs:image(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,image,Reason});
	Return -> Return
    end.


named_image(Name,Parent,Options) when is_list(Options) ->
    case gs:image(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_image,Reason});
	Return -> Return
    end.

    
arc(Parent,Options) when is_list(Options) ->
    case gs:arc(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,arc,Reason});
	Return -> Return
    end.


named_arc(Name,Parent,Options) when is_list(Options) ->
    case gs:arc(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_arc,Reason});
	Return -> Return
    end.

    
menu(Parent,Options) when is_list(Options) ->
    case gs:menu(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,menu,Reason});
	Return -> Return
    end.


named_menu(Name,Parent,Options) when is_list(Options) ->
    case gs:menu(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_menu,Reason});
	Return -> Return
    end.

    
menubutton(Parent,Options) when is_list(Options) ->
    case gs:menubutton(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,menubutton,Reason});
	Return -> Return
    end.


named_menubutton(Name,Parent,Options) when is_list(Options) ->
    case gs:menubutton(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_menubutton,Reason});
	Return -> Return
    end.

    
menubar(Parent,Options) when is_list(Options) ->
    case gs:menubar(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,menubar,Reason});
	Return -> Return
    end.


named_menubar(Name,Parent,Options) when is_list(Options) ->
    case gs:menubar(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_menubar,Reason});
	Return -> Return
    end.

    
menuitem(Parent,Options) when is_list(Options) ->
    case gs:menuitem(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,menuitem,Reason});
	Return -> Return
    end.


named_menuitem(Name,Parent,Options) when is_list(Options) ->
    case gs:menuitem(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_menuitem,Reason});
	Return -> Return
    end.

    
grid(Parent,Options) when is_list(Options) ->
    case gs:grid(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,grid,Reason});
	Return -> Return
    end.


named_grid(Name,Parent,Options) when is_list(Options) ->
    case gs:grid(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_grid,Reason});
	Return -> Return
    end.

    
gridline(Parent,Options) when is_list(Options) ->
    case gs:gridline(Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,gridline,Reason});
	Return -> Return
    end.


named_gridline(Name,Parent,Options) when is_list(Options) ->
    case gs:gridline(Name, Parent,Options) of
	{error, Reason} ->
	    exit({?MODULE,named_gridline,Reason});
	Return -> Return
    end.

    

%% gs:config - Utility functions


%%
%% enable/disable
%%

enable(Object) ->
    gse:config(Object,[{enable,true}]).

disable(Object) ->
    gse:config(Object,[{enable,false}]).



%%
%% select/deselect
%%

deselect(Object) ->
    gse:config(Object,[{select,false}]).

select(Object) ->
    gse:config(Object,[{select,true}]).


%%
%% map/unmap
%%

map(Object) ->
    gse:config(Object,[{map,true}]).

unmap(Object) ->
    gse:config(Object,[{map,false}]).



%%
%% resize
%%

resize(Object, Width, Height) ->
    gse:config(Object,[{width,Width}, {height, Height}]).


 
%%
%% Misc utility functions
%%

name_occupied(Name) ->
    case gs:read(Name,id) of
	{error,_Reason} ->
	     false;
	_Id -> true
    end.


