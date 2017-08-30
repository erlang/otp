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
%%
-module(dbg_wx_filedialog_win).

-behaviour(wx_object).

%% API
-export([new/3, getFilename/1, getFilenames/1, getDirectory/1, destroy/1]).

%% Internal 
-export([init/1, handle_call/3, handle_event/2, handle_cast/2,
	 handle_info/2, code_change/3, terminate/2]).

-include_lib("wx/include/wx.hrl").
-include("dbg_wx_filedialog_win.hrl").

-define(ID_PATH, 200).
-define(COMPLETION_WIN, 201).
-record(state, {win,
		back, forward, up, %% Buttons
		text,  %% Text (Path)
		ptext,
		icons=[],
		completion,
		list,  
		path, 
		files,
		rstack = [],
		fstack = [],
		filter,
		sort,
		cancel, 
		ok}).

-record(file, 
	{name = "",                %% File or dir name
	 type = "file",            %% Type descr
	 date = "",                %% Modification date
	 icon = 0,                 %% Icon
	 color = {0,0,0}
	}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client API


%% Options are:
%%     {message, Title}
%%     {defaultDir, Path}
%%     {filter, fun(Dir,File) -> skip | {DescriptionStr, icon_type}}
%%     {icons,  [{icon_type,wxBitmap}]}

new(Parent, Id, Options0) ->
    wx_object:start_link(?MODULE, [Parent, Id, Options0], []).

getFilename(FD) ->
    wx_object:call(FD, getFilename).
getFilenames(FD) ->
    wx_object:call(FD, getFilenames).
getDirectory(FD) ->
    wx_object:call(FD, getDirectory).
destroy(FD) ->
    wx_object:call(FD, destroy).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Object Callbacks
init([Parent, Id, Options0]) ->
    Name = proplists:get_value(message, Options0, "Open"),
    Size = proplists:get_value(size, Options0, ?wxDefaultSize),
    Pos  = proplists:get_value(pos, Options0, ?wxDefaultPosition),
    {ok, DefPath} = file:get_cwd(),
    Path = proplists:get_value(defaultDir, Options0, DefPath),
    ExtraIcons = proplists:get_value(icons, Options0, []),
    Filter = proplists:get_value(filter, Options0, fun file_type_and_icon/2),
    SortCol = sort_col(proplists:get_value(sort, Options0, name)),
    Dlg  = wxDialog:new(Parent, Id, Name, [{size,Size}, {pos,Pos}, 
					   {style, ?wxDEFAULT_DIALOG_STYLE 
						bor ?wxRESIZE_BORDER}]),
    
    %% Top 
    Back = wxButton:new(Dlg, ?wxID_BACKWARD),
    wxButton:disable(Back),
    Forw = wxButton:new(Dlg, ?wxID_FORWARD),
    wxButton:disable(Forw),
    Up   = wxButton:new(Dlg, ?wxID_UP),
    Dir  = wxTextCtrl:new(Dlg, ?ID_PATH, [{style, ?wxTE_PROCESS_ENTER}]),
    update_dir(Path, Dir),
    wxTextCtrl:connect(Dir, command_text_updated),
    wxTextCtrl:connect(Dir, command_text_enter),
    Self  = self(),
    IsTab = fun(Ev = #wx{event=#wxKey{keyCode=KC, 
				      controlDown=false,shiftDown=false, altDown=false}}, 
		_Object) when KC =:= ?WXK_TAB ; KC =:= ?WXK_ESCAPE ->
		    Self ! Ev;
	       (_Ev, Object) ->
		    %% Let the default handler handle anything else
		    wxEvent:skip(Object)
	    end,
    
    wxTextCtrl:connect(Dir, char, [{callback, IsTab}]),

    Top  = wxBoxSizer:new(?wxHORIZONTAL),
    _ = wxSizer:add(Top, Back, [{border, 2},{flag,?wxALL bor ?wxEXPAND}]),
    _ = wxSizer:add(Top, Forw, [{border, 2},{flag,?wxALL bor ?wxEXPAND}]),
    _ = wxSizer:add(Top, Up,   [{border, 2},{flag,?wxALL bor ?wxEXPAND}]),    

    %% List Ctrl
    {Art, IconMap} = create_icons(ExtraIcons),

    LC  = wxListCtrl:new(Dlg, [{style, ?wxLC_REPORT bor ?wxVSCROLL}, {size, {400,200}}]),
    wxListCtrl:assignImageList(LC, Art, ?wxIMAGE_LIST_SMALL),
    
    LI  = wxListItem:new(),
    Add = fun(MenuName, Row) ->
		  wxListItem:setText(LI, MenuName),
		  wxListItem:setAlign(LI, ?wxLIST_FORMAT_LEFT),
		  wxListCtrl:insertColumn(LC, Row, LI),
		  Row + 1		  
	  end,
    lists:foldl(Add, 0, ["Name", "Type", "Modified"]),
    wxListItem:destroy(LI),

    Files = list_files(Path, {SortCol,false}, Filter),
    update_files(Files,LC,IconMap),

    wxListCtrl:setColumnWidth(LC, 0, ?wxLIST_AUTOSIZE),
    wxListCtrl:setColumnWidth(LC, 1, ?wxLIST_AUTOSIZE),
    wxListCtrl:setColumnWidth(LC, 2, ?wxLIST_AUTOSIZE),
    wxListCtrl:connect(LC, command_list_item_activated),
    wxListCtrl:connect(LC, command_list_col_click),
    wxListCtrl:connect(LC, size, [{skip, true}]),

    %% Bottom buttons
    Bott = wxDialog:createButtonSizer(Dlg, ?wxCANCEL bor ?wxOK),
    wxDialog:connect(Dlg, command_button_clicked), 
    
    %% OK done
    Box  = wxBoxSizer:new(?wxVERTICAL),
    _ = wxSizer:add(Box, Top,  [{border, 2}, {flag,?wxALL bor ?wxEXPAND}]),
    _ = wxSizer:add(Box, Dir,  [{border, 2}, {flag,?wxALL bor ?wxEXPAND}]),
    _ = wxSizer:add(Box, LC,   [{border, 2}, {flag,?wxALL bor ?wxEXPAND}, {proportion, 1}]),
    _ = wxSizer:add(Box, Bott, [{border, 2}, {flag,?wxALL bor ?wxEXPAND}]),
    
    wxWindow:setSizer(Dlg, Box),
    _ = wxSizer:fit(Box, Dlg),
    wxSizer:setSizeHints(Box,Dlg),
    State = #state{win=Dlg, 
		   back=Back, forward=Forw, up=Up,
		   text=Dir,
		   list=LC, 
		   icons=IconMap,
		   sort ={SortCol,false},
		   filter = Filter,
		   path=Path,
		   files=Files
		  },
    {Dlg, State}.

%% calls
handle_call(getFilename, _From, State = #state{list=LC, files=Fs}) ->    
    case wxListCtrl:getNextItem(LC, -1, [{state,?wxLIST_STATE_SELECTED}]) of
	-1 ->
	    {reply, "", State};
	Item ->
	    {reply, (lists:nth(Item+1,Fs))#file.name, State}
    end;

handle_call(getFilenames, _From, State = #state{list=LC, files=Fs}) ->
    Items = get_selection(LC,-1, []),
    Files = [(lists:nth(Item+1,Fs))#file.name || Item <- Items],
    {reply, Files, State};

handle_call(getDirectory, _From, State = #state{path=Dir}) ->
    {reply, Dir, State};

handle_call(destroy, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

%%  events
handle_event(#wx{id=?wxID_UP}, State0) ->
    State = update_window(change_dir(0, State0)),
    {noreply, State};

handle_event(#wx{id=?wxID_BACKWARD}, State0 = #state{rstack=[Prev|Stack]}) ->
    State = update_window(change_dir(Prev, State0#state{rstack=Stack}, forward)),
    {noreply, State};

handle_event(#wx{id=?wxID_FORWARD}, State0 = #state{fstack=[Prev|Stack]}) ->
    State = update_window(change_dir(Prev, State0#state{fstack=Stack}, reverse)),
    {noreply, State};

handle_event(#wx{id=Id=?wxID_CANCEL}, State = #state{win=Win}) ->
    wxDialog:endModal(Win,Id),
    {noreply, State};

handle_event(#wx{id=Id=?wxID_OK}, State = #state{win=Win}) ->
    wxDialog:endModal(Win,Id),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Column0}}, 
	     State0 = #state{files=Fs, sort=Sort0}) ->
    case Column0 >= 0 of
	true ->
	    Column = sort_col(Column0+1),
	    Sort = case Sort0 of
		       {Column,Bool} -> {Column, not Bool};
		       {_,_} -> {Column, false}
		   end,
	    {noreply, update_window(State0#state{files=sort_files(Fs,Sort),sort=Sort})};
	false ->
	    {noreply, State0}
    end;
    
handle_event(#wx{event=#wxList{itemIndex=Index}}, 
	     State0 = #state{files=Fs,win=Win}) ->
    case lists:nth(Index+1, Fs) of
	#file{type="directory"} ->
	    State = update_window(change_dir(Index, State0)),
	    {noreply, State};
	_Dbg = #file{} ->
	    wxDialog:endModal(Win,?wxID_OK),
	    {noreply, State0}
    end;

handle_event(#wx{event=#wxCommand{type=command_text_updated, cmdString=Wanted}},
	     State = #state{ptext=Previous, completion=Comp}) ->
    case Previous =:= undefined orelse lists:prefix(Wanted, Previous) of
	true ->
	    destroy_completion(Comp),
	    {noreply, State#state{ptext=Wanted,completion=undefined}};
	false ->
	    {noreply, show_completion(Wanted, State)}
    end;

handle_event(#wx{event=#wxCommand{type=command_text_enter, cmdString=Wanted}}, 
	     State) ->
    case filelib:is_dir(Wanted, erl_prim_loader) of
	true ->
	    {Path0, Dir} = split_dir(Wanted),
	    Path = filename:join(Path0,Dir),
	    {noreply, update_window(change_dir(Path, State))};
	false ->
	    {Path, _} = split_dir(Wanted),
	    {noreply, update_window(change_dir(Path, State))}
    end;

handle_event(#wx{event=#wxKey{keyCode=?WXK_TAB}}, 
	     State = #state{text=TC, ptext=Wanted, completion=Comp}) ->
    case wxTextCtrl:getSelection(TC) of
	{Pos,Pos} ->
	    {noreply, show_completion(Wanted, State)};
	_ -> 
	    wxTextCtrl:setInsertionPointEnd(TC),
	    destroy_completion(Comp),
	    {noreply, State#state{completion=undefined}}
    end;

handle_event(#wx{id=?COMPLETION_WIN, event=#wxCommand{cmdString=[]}}, State) ->
    {noreply, State};
handle_event(#wx{id=?COMPLETION_WIN, obj=LB, 
		 event=_Ev=#wxCommand{cmdString=Dir,commandInt=N}}, 
	     State = #state{ptext=Wanted0, text=TC}) ->
    case wxListBox:isSelected(LB, N) of
	true ->
	    Wanted = case Wanted0 of
			 undefined -> wxTextCtrl:getValue(TC);
			 _ -> Wanted0
		     end,
	    Path1 =  case filelib:is_dir(Wanted, erl_prim_loader) of
			 true ->  Wanted;
			 false ->
			     {ThePath, _} = split_dir(Wanted),
			     ThePath
		     end,
	    Path = filename:join(Path1, Dir),
	    {noreply, update_window(change_dir(Path, State))};
	false ->
	    {noreply, State}
    end;

handle_event(#wx{event=#wxSize{size={Width,_}}}, State = #state{list=LC}) ->
    wx:batch(fun() ->
		     Tot = wx:foldl(fun(C,Sum) -> 
					    Sum + wxListCtrl:getColumnWidth(LC, C)
				    end, 0, [1,2]),
		     wxListCtrl:setColumnWidth(LC, 0, Width-Tot-30)
	     end),
    {noreply, State};
	
handle_event(_Event,State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply,State}.

terminate(_Reason,State) ->
    wxDialog:destroy(State#state.win),
    ok.

code_change(_,_,State) ->
    State.

%%%%%%%%%%%%%%%%%%%%%%% Internal %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_window(State = #state{files=Fs,  list=LC,
			     path=Path, text=TC,
			     icons=Icons,
			     completion = Comp}) ->
    update_files(Fs, LC, Icons),
    update_dir(Path, TC),
    if State#state.rstack == [] -> wxButton:disable(State#state.back);
       true -> wxButton:enable(State#state.back)
    end,
    if State#state.fstack == [] -> wxButton:disable(State#state.forward);
       true -> wxButton:enable(State#state.forward)
    end,
    if Path == "/" -> wxButton:disable(State#state.up);
       true -> wxButton:enable(State#state.up)
    end,
    destroy_completion(Comp),
    State#state{completion=undefined, ptext=undefined}.

update_dir(Path, TC) ->
    case Path of
	"/" -> 
	    wxTextCtrl:setValue(TC, Path);
	_ -> 
	    wxTextCtrl:setValue(TC, Path ++ "/")
    end,
    wxTextCtrl:setInsertionPointEnd(TC).

update_files(Files,LC, Icons) ->
    wxListCtrl:deleteAllItems(LC),
    wx:foldl(fun(F=#file{name=Name,type=TypeStr,date=Date, color=Color},Row) ->
		     wxListCtrl:insertItem(LC, Row, ""), 
		     if (Row rem 2) =:= 0 -> 
			     wxListCtrl:setItemBackgroundColour(LC, Row, {240,240,255});
			true -> ignore
		     end,
		     wxListCtrl:setItemTextColour(LC, Row, Color),
		     wxListCtrl:setItem(LC, Row, 0, Name, [{imageId,get_icon(F,Icons)}]),
		     wxListCtrl:setItem(LC, Row, 2, format_date(Date)),
		     wxListCtrl:setItem(LC, Row, 1, TypeStr),
		     Row+1
	     end, 0, Files).

show_completion(undefined, State = #state{text=TC}) ->
    show_completion(wxTextCtrl:getValue(TC), State);
show_completion(Wanted, State = #state{text=TC, win=Win, list=LC, completion=Comp}) ->
    Paths0 = filelib:wildcard(Wanted ++ "*", erl_prim_loader),
    Paths = [File || File <- Paths0, filelib:is_dir(File, erl_prim_loader)],
    case Paths of
	[Path] ->
	    Start = length(Wanted),
	    wxTextCtrl:setValue(TC, Path++"/"),
	    wxTextCtrl:setInsertionPoint(TC, Start),
	    Last = wxTextCtrl:getLastPosition(TC),
	    wxTextCtrl:setSelection(TC, Start, Last),
	    destroy_completion(Comp),
	    State#state{ptext=Path, completion=undefined};
	Paths when Comp =:= undefined ->
	    {PosX,PosY} = wxListCtrl:getPosition(LC),
	    {SzX, SzY}  = wxListCtrl:getSize(LC),
	    Pos0  = {PosX+5,PosY},
	    Size = {SzX-50,SzY-50},
	    Files = [filename:basename(File) || File <- Paths],
	    Temp = case os:type() of
		       {win32,nt} ->
			   Pos = wxWindow:clientToScreen(Win,Pos0),
			   wxFrame:new(Win, -1, "", 
				       [{pos, Pos}, {size, Size},
					{style, ?wxFRAME_FLOAT_ON_PARENT}]);
		       _ ->
			   wxWindow:new(Win, -1, 
					[{pos, Pos0}, {size, Size},
					 {style, ?wxFRAME_FLOAT_ON_PARENT}])
		   end,
	    LB = wxListBox:new(Temp, ?COMPLETION_WIN, 
			       [{style, ?wxLB_SINGLE}, {choices, Files}, {size, Size}]),

	    %% wxListBox:connect(LB, command_listbox_doubleclicked),
	    wxListBox:connect(LB, command_listbox_selected),
	    wxWindow:show(Temp),
	    %% setFocus does a select all on 2.9 sigh..
	    {Start, Last} = wxTextCtrl:getSelection(TC),
	    wxWindow:setFocus(TC),
	    wxTextCtrl:setSelection(TC, Start, Last),
	    State#state{completion = {Temp, LB}, ptext=Wanted};
	Paths ->
	    {_Temp, LB} = Comp,
	    wxListBox:clear(LB),
	    Files = [filename:basename(File) || File <- Paths],
	    Files /= [] andalso wxListBox:insertItems(LB,Files,0),
	    State#state{ptext=Wanted}
    end.

destroy_completion(undefined) -> ok;
destroy_completion({Window, _LB}) ->
    Parent = wxWindow:getParent(Window),
    wxWindow:hide(Window),
    wxWindow:destroy(Window),
    wxWindow:refresh(Parent).

split_dir(Path0) ->
    Split1 = filename:split(Path0),
    case lists:reverse(Split1) of
	[File| Split2] when Split2 =/= [] ->	    
	    Split3 = lists:reverse(Split2),
	    Path = filename:join(Split3),
	    {Path, File};
	_ ->
	    {"/", ""}
    end.
   
change_dir(What,State) ->
    change_dir(What,State,new).

change_dir(Num, State = #state{files=Fs0, path=Path0},Stack) 
  when is_integer(Num) ->
    case lists:nth(Num+1, Fs0) of
	#file{name=".."} ->
	    {Path,_} = split_dir(Path0);
	#file{name=Dir} ->
	    Path = filename:join(Path0, Dir)
    end,
    change_dir(Path, State, Stack);
change_dir(Path, State0 = #state{path=Path0, sort=Sort, filter=Filter},StackDir) ->
    Files = list_files(Path, Sort, Filter),
    add_to_stack(StackDir, Path0, State0#state{files=Files, path=Path}).

add_to_stack(new, Path, State = #state{rstack=Stack0}) ->
    Stack = [Path|Stack0],
    State#state{rstack=Stack, fstack=[]};
add_to_stack(reverse, Path, State = #state{rstack=Stack0}) ->
    Stack = [Path|Stack0],
    State#state{rstack=Stack};
add_to_stack(forward, Path, State = #state{fstack=Stack0}) ->
    Stack = [Path|Stack0],
    State#state{fstack=Stack}.

list_files(Dir, Sort, Filter) ->
    Contents0 = filelib:wildcard(Dir ++ "/*", erl_prim_loader),
    Contents = case Dir of
		   "/" -> Contents0;
		   _   -> [".."|Contents0]
	       end,
    {Ds0,Fs0} = get_file_info(Contents, Dir, Filter, [], []),
    sort_files(lists:reverse(Ds0), Fs0, Sort).

sort_files(Mixed, Sort) ->
    {Ds,Fs} = 
	lists:foldr(fun(Dir = #file{type="directory"}, {Ds,Fs}) ->
			    {[Dir|Ds],Fs};
		       (File, {Ds,Fs}) ->
			    {Ds,[File|Fs]}
		    end, {[],[]}, Mixed),
    sort_files(Ds,Fs,Sort).

sort_files(Ds0, Fs0, {SortElement, Rev}) ->
    {Top,Ds1} = case Ds0 of
		    [Up=#file{name=".."}|Rest] -> {Up,Rest};
		    _ -> {undefined, Ds0}
		end,
    Ds = lists:keysort(SortElement, Ds1),
    Fs = case Rev of 
	     true ->  lists:reverse(lists:keysort(SortElement,Fs0));
	     false -> lists:keysort(SortElement,Fs0)
	 end,
    case Top of
	undefined ->  Ds ++ Fs;
        _ ->          [Top|Ds++Fs]
    end.
    
get_file_info([AbsName|Rest],Dir,Filter, Files,Dirs) ->
    Name    = filename:basename(AbsName),
    Mod     = filelib:last_modified(AbsName, erl_prim_loader),
    IsDir   = filelib:is_dir(AbsName, erl_prim_loader),
    Entry0 = #file{name=Name, date=Mod},
    case IsDir of 
	true when Name =:= ".." ->
	    Entry = Entry0#file{type="directory",icon=prev_dir},
	    get_file_info(Rest, Dir, Filter, Files, [Entry|Dirs]);
	true ->
	    Entry = Entry0#file{type="directory",icon=dir},
	    get_file_info(Rest, Dir, Filter, Files,  [Entry|Dirs]);
	false ->
	    case Filter(Dir,Name) of
		{Type,Icon,Color} ->
		    Entry = Entry0#file{type=Type,icon=Icon,color=Color},
		    get_file_info(Rest, Dir, Filter, [Entry|Files], Dirs);
		skip ->
		    get_file_info(Rest, Dir, Filter, Files, Dir)
	    end
    end;
get_file_info([], _, _, Fs,Ds) ->
    {Ds,Fs}.

format_date({{Y,M,D},{H,Mi,S}}) ->
    lists:flatten(io_lib:format("~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Y,M,D,H,Mi,S]));
format_date(_) ->
    "unknown".

get_selection(LC, Prev, Acc) ->
    case wxListCtrl:getNextItem(LC, Prev, [{state,?wxLIST_STATE_SELECTED}]) of
	-1 -> 
	    lists:reverse(Acc);
	Item ->
	    get_selection(LC, Item, [Item|Acc])
    end.

file_type_and_icon(_Dir, Name) ->
    case filename:extension(Name) of 
	".erl" ->
	    {"erl src", erl_src, {0,90,0}};
	".hrl" ->
	    {"erl hrl", erl_hrl, {0,90,0}};
	".beam" ->
	    {"erl bin", erl_bin, {0,0,0}};
	_ ->
	    {"file",    file,    {0,0,0}}
    end.

create_icons(Extra) ->    
    Art = wxImageList:new(16,16),
    BuiltIn0 = [{file, "wxART_NORMAL_FILE"}, 
	       {dir, "wxART_FOLDER"}, 
	       {prev_dir, "wxART_GO_DIR_UP"}],
    BuiltIn = [{Type, wxArtProvider:getBitmap(ArtID, [{size, {16,16}}])} || 
		  {Type,ArtID} <- BuiltIn0],

    Test = [{Type, wxBitmap:new(wxImage:new(16,16,Bin))} 
	    || {Type,Bin} <- 
		   [{erl_src, ?ERL_SRC},
		    {erl_hrl, ?ERL_HRL},
		    {erl_bin, ?ERL_BIN}]],
    
    Icons = BuiltIn ++ Test ++ Extra,
    [wxImageList:add(Art, Image) || {_, Image} <- Icons],
    {Art, ids(Icons, 0)}.

get_icon(#file{icon=Icon}, Icons) ->
    proplists:get_value(Icon,Icons, 0).

ids([{Type,_}|Rest], Id) ->
    [{Type,Id}|ids(Rest,Id+1)];
ids([],_) -> [].
		      
sort_col(1) -> #file.name;
sort_col(2) -> #file.type;
sort_col(3) -> #file.date;
sort_col(name) -> #file.name;
sort_col(type) -> #file.type;
sort_col(date) -> #file.date.
    
