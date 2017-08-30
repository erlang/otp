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
%% Widget specific data
%% ------------------------------------------------------------
%%

-module(gstk_widgets).

-export([type2mod/1, objmod/1, suffix/1]).

-include("gstk.hrl").




%%
%% Map primitive types to modules or false (false should not be a module!)
%%
%% ordered for efficiency

type2mod(window)      -> gstk_window;
type2mod(frame)       -> gstk_frame;
type2mod(button)      -> gstk_button;
type2mod(canvas)      -> gstk_canvas;
type2mod(checkbutton) -> gstk_checkbutton;
type2mod(rectangle)   -> gstk_rectangle;
type2mod(gs)          -> gstk_gs;
type2mod(grid)        -> gstk_grid;
type2mod(gridline)    -> gstk_gridline;
type2mod(text)        -> gstk_text;
type2mod(image)       -> gstk_image;
type2mod(label)       -> gstk_label;
type2mod(line)        -> gstk_line;
type2mod(entry)       -> gstk_entry;
type2mod(listbox)     -> gstk_listbox;
type2mod(editor)      -> gstk_editor;
type2mod(menu)        -> gstk_menu;
type2mod(menubar)     -> gstk_menubar;
type2mod(menubutton)  -> gstk_menubutton;
type2mod(menuitem)    -> gstk_menuitem;
type2mod(message)     -> gstk_message;
type2mod(oval)        -> gstk_oval;
type2mod(polygon)     -> gstk_polygon;
type2mod(prompter)    -> gstk_prompter;
type2mod(radiobutton) -> gstk_radiobutton;
type2mod(scale)       -> gstk_scale;
type2mod(scrollbar)   -> gstk_scrollbar;
type2mod(arc)         -> gstk_arc;
type2mod(Type)        -> {error,{unknown_type, Type}}.

objmod(#gstkid{objtype=OT}) -> type2mod(OT).

%%
%% The suffix to add to the parent tk widget
%%
suffix(button)       -> ".b";
suffix(canvas)       -> ".c";
suffix(checkbutton)  -> ".cb";
suffix(editor)       -> ".ed";
suffix(entry)        -> ".e";
suffix(frame)        -> ".f";
suffix(label)        -> ".l";
suffix(listbox)      -> ".lb";
suffix(menu)         -> ".m";
suffix(menubar)      -> ".bar";
suffix(menubutton)   -> ".mb";
suffix(message)      -> ".ms";
suffix(prompter)     -> ".p";
suffix(radiobutton)  -> ".rb";
suffix(scale)        -> ".sc";
suffix(window)       -> ".w";
suffix(Objtype) -> apply(type2mod(Objtype), suffix, []).


