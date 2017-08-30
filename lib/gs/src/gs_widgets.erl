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

-module(gs_widgets).


%% ----- Exports -----
-export([default_options/1,
	 container/1]).


%% ------------------------------------------------------------
%% default_options for widgets
%% Keep the options in the list sorted!
%% ------------------------------------------------------------

default_options(arc)         -> [{coords, [{0,0}, {0,0}]}];
default_options(button)      -> [{click,true}, {height,30}, {width,100}, {x,0},
				{y,0}];
default_options(canvas)      -> [{height,200}, {scrollregion,{0,0,300,200}},
				 {width,300}, {x,0}, {y,0}];
default_options(checkbutton) -> [{click,true}, {height,30}, {width,100}, {x,0},
				 {y,0}];
default_options(editor)      -> [{height,200}, {width,300}, {x,0}, {y,0}];
default_options(entry)       -> [{height,30}, {width,100}, {x,0}, {y,0}];
default_options(frame)       -> [{height,100}, {width,150}, {x,0}, {y,0}];
default_options(grid)        -> [{bg,grey}, {cellheight,20},
				 {columnwidths, [80,80,80,80]},
				 {fg,black}, {font,{screen, 12}},
				 {height,100},
				 {hscroll,bottom},
				 {rows,{1,10}},
				 {vscroll,right},
				 {width,300},
		 		 {x,0}, {y,0}];
						           % Keep the options in the list sorted!
default_options(gridline)    -> [{click,true}, {doubleclick,false}, {row,undefined}];
default_options(gs)          -> [{kernel,false},
				 {{default,all,font}, {screen,12}}];
default_options(image)       -> [{anchor,nw}, {coords,[{0,0}]}];
default_options(label)       -> [{height,30}, {width,100}, {x,0}, {y,0}];
default_options(line)        -> [{coords, [{-1,-1},{-1,-1}]}];
default_options(listbox)     -> [{height,130}, {hscroll,true},
				 {selectmode,single}, {vscroll,true},
				 {width,125}, {x,0}, {y,0}];
default_options(menu)        -> [];
						           % Keep the options in the list sorted!
default_options(menubar)     -> [{bw,2}, {height,25}, {highlightbw,0},
				 {relief,raised}];
default_options(menubutton)  -> [{anchor,nw}, {side,left}];
default_options(menuitem)    -> [{click,true}, {index,last}, {itemtype,normal}];
default_options(message)     -> [{height,75}, {width,100}];
default_options(oval)        -> [{coords, [{0,0},{0,0}]}];
default_options(polygon)     -> [{coords, [{0,0},{0,0}]}, {fg,black}, {fill,none}];
default_options(prompter)    -> [{height,200}, {prompt,[]}, {width,300}];
default_options(radiobutton) -> [{click,true}, {height,30}, {width,100},
				 {x,0}, {y,0}];
default_options(rectangle)   -> [{coords, [{0,0},{0,0}]}];
default_options(scale)       -> [{click,true}, {height,50}, {width,100},
				 {x,0}, {y,0}];
						           % Keep the options in the list sorted!
default_options(scrollbar)   -> [];
default_options(text)        -> [{anchor,nw}, {coords,[{0,0}]}, {justify,left}];
default_options(window)      -> [{configure,false}, {cursor,arrow}, {destroy,true},
				 {height,200}, {map,false}, {width,300}];
default_options(_)           -> [].

container(canvas)      -> true;
container(frame)       -> true;
container(grid)        -> true;
container(menu)        -> true;
container(menubar)     -> true;
container(menubutton)  -> true;
container(menuitem)    -> true;
container(window)      -> true;
container(_)           -> false.
