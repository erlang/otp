%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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
-module(dbg_wx_win).

%% External exports
-export([init/0,
	 create_menus/4, %% For wx
	 add_break/3, update_break/2, delete_break/1,
	 motion/2,
	 confirm/2, notify/2, entry/4, open_help/2,
	 to_string/1, to_string/2,
	 find_icon/1
	]).

-record(break, {mb, smi, emi, dimi, demi}).
-include_lib("wx/include/wx.hrl").

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% init() -> GS
%%   GS = term()
%%--------------------------------------------------------------------
init() ->
    _ = wx:new(),
    ok.

%%--------------------------------------------------------------------
%% create_menus(MenuBar, [Menu])
%%   MenuBar = gsobj()
%%   Menu = {Name, [Item]}
%%     Name = atom()
%%     Item = {Name, N} | {Name, N, Type} | {Name, N, cascade, [Item]}
%%          | separator
%%       N = no | integer()
%%       Type = check | radio
%% Create the specified menus and menuitems.
%%
%% Normal menuitems are specified as {Name, N}. Generates the event:
%%   {gs, _Id, click, {menuitem, Name}, _Arg}
%%
%% Check and radio menuitems are specified as {Name, N, check|radio}.
%% They are assumed to be children to a cascade menuitem! (And all children
%% to one cascade menuitem are assumed to be either check OR radio
%% menuitems)!
%% Selecting a check/radio menuitem generates the event:
%%   {gs, _Id, click, {menu, Menu}, _Arg}
%% where Menu is the name of the parent, the cascade menuitem.
%% Use selected(Menu) to retrieve which check/radio menuitems are
%% selected.
%%--------------------------------------------------------------------

create_menus(MB, [{Title,Items}|Ms], Win, Id0) ->
    Menu = wxMenu:new([]),
    put(Title,Menu),
    Id = create_menu_item(Menu, Items, Win, Id0, true),
    wxMenuBar:append(MB,Menu,menu_name(Title,ignore)),
    create_menus(MB,Ms,Win,Id);
create_menus(_MB,[], _Win,Id) ->
    Id.

create_menu_item(Menu, [separator|Is], Win, Id,Connect) ->
    _ = wxMenu:appendSeparator(Menu),
    create_menu_item(Menu,Is,Win,Id+1,Connect);
create_menu_item(Menu, [{Name, _N, cascade, Items}|Is], Win, Id0,Connect) ->
    Sub = wxMenu:new([]),
    Id = create_menu_item(Sub, Items, Win, Id0, false),
    _ = wxMenu:append(Menu, ?wxID_ANY, menu_name(Name,ignore), Sub),
    %% Simulate GS sub checkBox/RadioBox behaviour
    Self = self(),
    Butts = [{MI,get(MI)} || {MI,_,_} <- Items],
    IsChecked = fun({MiName,MI},Acc) ->
			case wxMenuItem:isChecked(MI) of
			    true -> [MiName|Acc];
			    false -> Acc
			end		
		end,
    Filter = fun(Ev,_) ->
		     Enabled = lists:foldl(IsChecked, [], Butts),
		     Self ! Ev#wx{userData={Name, Enabled}}
	     end,
    _ = wxMenu:connect(Win, command_menu_selected,
		       [{id,Id0},{lastId, Id-1},{callback,Filter}]),
    create_menu_item(Menu, Is, Win, Id, Connect);
create_menu_item(Menu, [{Name,Pos}|Is], Win, Id, Connect) -> 
    MenuId = case lists:member(Name, ['Debugger']) of
		 true -> ?wxID_HELP;
		 _ ->    Id
	     end,
    Item = wxMenu:append(Menu, MenuId, menu_name(Name,Pos)),
    put(Name,Item),
    if Connect -> 
	    wxMenu:connect(Win, command_menu_selected, [{id,MenuId},{userData, Name}]);
       true -> ignore
    end,
    create_menu_item(Menu,Is,Win,Id+1, Connect);
create_menu_item(Menu, [{Name,N,check}|Is], Win, Id, Connect) ->
    Item = wxMenu:appendCheckItem(Menu, Id, menu_name(Name,N)),
    put(Name,Item),
    if Connect -> 
	    wxMenu:connect(Win, command_menu_selected, [{id,Id},{userData, Name}]);
       true -> ignore
    end,
    create_menu_item(Menu,Is,Win,Id+1,Connect);
create_menu_item(Menu, [{Name, N, radio}|Is], Win, Id,Connect) ->
    Item = wxMenu:appendRadioItem(Menu, Id, menu_name(Name,N)),
    put(Name,Item),
    if Connect -> 
	    wxMenu:connect(Win, command_menu_selected, [{id,Id},{userData, Name}]);
       true -> ignore
    end,
    create_menu_item(Menu,Is,Win,Id+1,Connect);
create_menu_item(_, [], _, Id,_) ->
    Id.

%%--------------------------------------------------------------------
%% add_break(Name, Point) -> #break{}
%%   Name = atom()
%%   Point = {Mod, Line}
%% The break will generate the following events:
%%   #wx{userData= {break, Point, Event}}
%%     Event = delete | {trigger, Action} | {status, Status}
%%       Action = enable | disable | delete
%%       Status = active | inactive
%%--------------------------------------------------------------------
add_break(Win, MenuName, Point) ->
    %% Create a name for the breakpoint
    {Mod, Line} = Point,
    Label = to_string("~w ~5w", [Mod, Line]),

    Menu = get(MenuName),
    %% Create a menu for the breakpoint
    Add = fun(Item,Action) ->
		  Id = wxMenuItem:getId(Item),
		  wxMenu:connect(Win, command_menu_selected, 
				 [{id,Id}, {userData, Action}])
	  end,
    Sub = wxMenu:new([]),
    Dis = wxMenu:append(Sub, ?wxID_ANY, "Disable"),
    Add(Dis, {break,Point,status}),
    Del = wxMenu:append(Sub, ?wxID_ANY, "Delete"),
    Add(Del, {break,Point,delete}),
    Trigger = wxMenu:new([]),
    Enable = wxMenu:appendRadioItem(Trigger, ?wxID_ANY,"Enable"),
    Add(Enable, {break,Point,{trigger,enable}}),
    TDisable = wxMenu:appendRadioItem(Trigger, ?wxID_ANY,"Disable"),
    Add(TDisable, {break,Point,{trigger,disable}}),
    Delete = wxMenu:appendRadioItem(Trigger, ?wxID_ANY,"Delete"),
    Add(Delete, {break,Point,{trigger,delete}}),

    _ = wxMenu:append(Sub, ?wxID_ANY, "Trigger Action", Trigger),
    MenuBtn = wxMenu:append(Menu,?wxID_ANY, Label, Sub),

    #break{mb={Menu,MenuBtn}, 
	   smi=Dis, emi=Enable, dimi=TDisable, demi=Delete}.

%%--------------------------------------------------------------------
%% update_break(Break, Options)
%%   Break = #break{}
%%   Options = [Status, Action, Mods, Cond]
%%     Status = active | inactive
%%     Action = enable | disable | delete
%%     Mods = null (not used)
%%     Cond = null | {Mod, Func}
%%--------------------------------------------------------------------
update_break(Break, Options) ->
    [Status, Trigger|_] = Options,

    Label = case Status of
		active -> "Disable";
		inactive -> "Enable"
	    end,
    wxMenuItem:setText(Break#break.smi, Label),

    TriggerMI = case Trigger of
		    enable -> Break#break.emi;
		    disable -> Break#break.dimi;
		    delete -> Break#break.demi
		end,
    wxMenuItem:check(TriggerMI).

%%--------------------------------------------------------------------
%% delete_break(Break)
%%   Break = #break{}
%%--------------------------------------------------------------------
delete_break(Break) ->
    {Menu, MenuBtn} = Break#break.mb,
    wxMenu:'Destroy'(Menu,MenuBtn).

%%--------------------------------------------------------------------
%% motion(X, Y) -> {X, Y}
%%   X = Y = integer()
%%--------------------------------------------------------------------
motion(X, Y) ->
    receive
	{gs, _Id, motion, _Data, [NX,NY]} ->
	    motion(NX, NY)
    after 0 ->
	    {X, Y}
    end.


%%--------------------------------------------------------------------
%% confirm(MonWin, String) -> ok | cancel
%%--------------------------------------------------------------------

confirm(Win,Message) ->
    MD = wxMessageDialog:new(Win,to_string(Message),
			     [{style, ?wxOK bor ?wxCANCEL}, 
			      {caption, "Confirm"}]),
    Res = case wxDialog:showModal(MD) of
	      ?wxID_OK -> ok;
	      _ -> cancel
	  end,
    wxDialog:destroy(MD),
    Res.

%%--------------------------------------------------------------------
%% notify(MonWin, String) -> ok 
%%--------------------------------------------------------------------

notify(Win,Message) ->
    MD = wxMessageDialog:new(Win,to_string(Message),
			     [{style, ?wxOK}, 
			      {caption, "Confirm"}]),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD),
    ok.

%%--------------------------------------------------------------------
%% entry(Parent, Title, Prompt, {Type, Value}) -> {Prompt, Val} | cancel
%%--------------------------------------------------------------------

entry(Parent, Title, Prompt, {Type, Value}) ->
    Ted = wxTextEntryDialog:new(Parent, to_string(Prompt),
				[{caption, to_string(Title)},
				 {value, Value}]),

    case wxDialog:showModal(Ted) of
	?wxID_OK ->
	    Res = case verify(Type, wxTextEntryDialog:getValue(Ted)) of
		      {edit,NewVal} ->
			  {Prompt,NewVal};
		      ignore ->
			  cancel
		  end,
	    wxTextEntryDialog:destroy(Ted),
	    Res;
	_ ->
	    cancel
    end.


verify(Type, Str) ->
    case erl_scan:string(Str, 1, [text]) of
	{ok, Tokens, _EndLine} when Type==term ->
	    case erl_eval:extended_parse_term(Tokens++[{dot, erl_anno:new(1)}]) of
		{ok, Value} -> {edit, Value};
		_Error -> 
		    ignore
	    end;
	{ok, [{Type, _Line, Value}], _EndLine} when Type/=term ->
	    {edit, Value};
	_Err ->
	    ignore
    end.

%%--------------------------------------------------------------------
%% open_help/2
%%    opens browser with help file
%%--------------------------------------------------------------------
open_help(_Parent, HelpHtmlFile) ->
    wx_misc:launchDefaultBrowser("file://" ++ HelpHtmlFile).

%%--------------------------------------------------------------------
%% to_string(Term) -> [integer()]
%% to_string(Format,Args) -> [integer()]
%%--------------------------------------------------------------------
    
to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
to_string([]) -> "";
to_string(List) when is_list(List) ->
    List;
to_string(Term) ->
    io_lib:format("~tp",[Term]).

to_string(Format,Args) ->
    io_lib:format(Format, Args).

menu_name(Atom, N) when is_atom(Atom) -> 
    menu_name(atom_to_list(Atom),N);
menu_name("Help", _) -> %% Mac needs this to be exactly this
    "&Help";
menu_name(Str, Pos) when is_integer(Pos) ->
    {S1,S2} = lists:split(Pos,Str),
    S1 ++ [$&|S2];
menu_name(Str,_) ->
    Str.

%%--------------------------------------------------------------------
%% find_icon(File) -> Path or exists
%%--------------------------------------------------------------------

find_icon(File) ->
    PrivDir = code:priv_dir(debugger),
    PrivIcon = filename:append(PrivDir, File),
    case filelib:is_regular(PrivIcon) of
	true -> PrivIcon;
	false -> 
	    CurrDir = filename:dirname(code:which(?MODULE)),	    
	    CurrIcon = filename:append(CurrDir, File),
	    true = filelib:is_regular(CurrIcon),
	    CurrIcon
    end.

