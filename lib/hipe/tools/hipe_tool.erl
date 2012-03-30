%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2012. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2002 by Erik Johansson.  
%% ====================================================================
%%  Module   :	hipe_tool
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2002-03-13 Erik Johansson (happi@it.uu.se): Created.
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_tool).
-compile([{nowarn_deprecated_function,{gs,button,3}},
          {nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,destroy,1}},
          {nowarn_deprecated_function,{gs,editor,3}},
          {nowarn_deprecated_function,{gs,label,3}},
          {nowarn_deprecated_function,{gs,listbox,3}},
          {nowarn_deprecated_function,{gs,menu,3}},
          {nowarn_deprecated_function,{gs,menubar,3}},
          {nowarn_deprecated_function,{gs,menubutton,3}},
          {nowarn_deprecated_function,{gs,menuitem,2}},
          {nowarn_deprecated_function,{gs,start,0}},
          {nowarn_deprecated_function,{gs,window,3}}]).

-export([start/0]).

%%---------------------------------------------------------------------

-include("../main/hipe.hrl").

%%---------------------------------------------------------------------

-define(WINDOW_WIDTH, 920).
-define(WINDOW_HEIGHT, 460).
-define(DEFAULT_BG_COLOR, {217,217,217}).
-define(POLL_INTERVAL, 5000).
-define(FONT, {screen, 12}).
-define(HEADER_FONT, {screen, [bold], 12}).
-define(NORMAL_FG_COLOR, {0,0,0}).

%%---------------------------------------------------------------------

-type fa() :: {atom(), arity()}. % {Fun,Arity}
-type fa_address() :: {atom(), arity(), non_neg_integer()}. % {F,A,Address}

%%---------------------------------------------------------------------

-record(state, {win_created = false	:: boolean(),
		mindex = 0		:: integer(),
		mod			:: atom(),
		funs = []		:: [fa()],
		mods = [] 		:: [atom()],
		options = [o2]		:: comp_options(),
		compiling = false	:: 'false' | pid()
	       }).

%%---------------------------------------------------------------------

-spec start() -> pid().

start() ->
  spawn(fun () -> init() end).

init() ->
  process_flag(trap_exit, true), 
  gs:start(),
  S = init_window(#state{}),
  loop(S).

-spec loop(#state{}) -> no_return().

loop(State) ->
    receive
      {gs, code_listbox, click, Data, [Idx, Txt | _]} ->
	NewState = update_module_box(State,Idx,Data,Txt),
	loop(NewState);
      {gs, module_listbox, click, Data, [Idx, _Txt | _]} ->
	NewState = update_fun(State,Idx,Data),
	loop(NewState);
      {gs, compmod, click, _, _} ->
	loop(compile(State));
      {gs, prof, click, [], ["Turn off\nProfiling"]} ->
	hipe_profile:prof_module_off(State#state.mod),
	loop(update_module_box(State,State#state.mindex,State#state.mods,""));
      {gs, prof, click, [], _} ->
	hipe_profile:prof_module(State#state.mod),
	loop(update_module_box(State,State#state.mindex,State#state.mods,""));
      {gs, win, configure, _, _} ->
	gs:config(win, [{width, ?WINDOW_WIDTH}, {height, ?WINDOW_HEIGHT}]),
	loop(State);

      show_window when State#state.win_created =:= true ->
	gs:config(win, [raise]),
	loop(State);
      show_window when State#state.win_created =:= false ->
	loop((init_window(State))#state{win_created = true});

      {gs, _Id, click, close_menu, _Args} ->
	gs:destroy(win),
	loop(State#state{win_created = false});
      {gs, _Id, keypress, _Data, [c, _, 0, 1 | _]} ->
	gs:destroy(win),
	loop(State#state{win_created = false});
      {gs, _Id, keypress, _Data, ['C', _, 1, 1 | _]} ->
	gs:destroy(win),
	loop(State#state{win_created = false});
      {gs, _Id, keypress, _Data, _Args} ->
	loop(State);
      {gs, _, destroy, _, _} ->
	loop(State#state{win_created = false});

      {compilation_done, _Res, Sender} ->
	case State#state.compiling of
	  Sender ->
	    catch gs:config(compmod, [{enable, true}]),
	    update_text(compiling, ""),
	    loop(update_module_box(State,
				   State#state.mindex,
				   State#state.mods, ""));
	  _ ->
	    loop(State)
	end;

      {'EXIT', _Pid, _Reason} ->
	exit(normal);
      _Other ->
	io:format("HiPE window received message ~p ~n", [_Other]),
	loop(State)
    after 
      ?POLL_INTERVAL ->
	loop(update_code_listbox(State))
    end.

-spec init_window(#state{}) -> #state{}.

init_window(State) ->
  create_window(State),
  gs:config(win, [{map,true}]),
  update_code_listbox(State#state{win_created = true}).

-spec create_window(#state{}) -> 'ok'.

create_window(State) ->
  gs:window(win, gs:start(), [{width, ?WINDOW_WIDTH},
			      {height, ?WINDOW_HEIGHT},
			      {bg, ?DEFAULT_BG_COLOR},
			      {title, "[HiPE] Code list"},
			      {configure, true},
			      {destroy, true},
			      {cursor, arrow},
			      {keypress, true}
			     ]),
  create_menu(),
  Xpos = 4, 
  Ypos1 = 60, 
  Width =  (?WINDOW_WIDTH - (Xpos*4)) div 3,
  create_labels([{mods,Ypos1-20,"Loaded Modules"}], Xpos + 1 + 3),
  Xpos2 = Xpos*2+Width,
  create_labels([{mod,Ypos1-20,"Module:"++atom_to_list(State#state.mod)},
		 {ver,Ypos1,""},
		 {time,Ypos1+20,""},
		 {native,Ypos1+40,""},
		 {compiling,Ypos1+60,""}], Xpos2),
  create_labels([{function,Ypos1-20,"Function:"},
		 {nativefun,Ypos1,""}], Xpos*3+Width*2),
  Ypos = 240,
  Height1 = ?WINDOW_HEIGHT - Ypos1 - Xpos,
  Height = ?WINDOW_HEIGHT - Ypos - Xpos,
  gs:listbox(code_listbox, win, [{x, Xpos},
				 {y, Ypos1},
				 {width, Width},
				 {height, Height1},
				 {bg, {255,255,255}},
				 {vscroll, right},
				 {hscroll, true},
				 {click, true}]),
  gs:listbox(module_listbox, win, [{x, Xpos*2+Width},
				   {y, Ypos},
				   {width, Width},
				   {height, Height},
				   {bg, {255,255,255}},
				   {vscroll, right},
				   {hscroll, true},
				   {click, true}]),
  gs:listbox(profile_listbox, win, [{x, Xpos*3+Width*2},
				    {y, Ypos1+40},
				    {width, Width},
				    {height, Height-60},
				    {bg, {255,255,255}},
				    {vscroll, right},
				    {hscroll, true},
				    {click, true}]),
  gs:button(compmod,win,[{label,{text,"Compile\nModule"}},
			 {justify,center},
			 {x,Xpos*2+Width*1},
			 {height,60},
			 {y,Ypos-80}]),
  gs:button(prof,win,[{label,{text,"Profile\nModule"}},
		      {justify,center},
		      {x,Xpos*2+Width*1+100},
		      {height,60},
		      {y,Ypos-80}]),
  gs:button(clearprof,win,[{label, {text,"Clear\nProfile"}},
			   {justify, center},
			   {x, Xpos*2+Width*1+200},
			   {height, 60},
			   {y, Ypos-80}]),
  gs:editor(edoc,win,[{x, Xpos*3+Width*2}, {y, Ypos},
		      {width, Width}, {height, Height},
		      {insert, {'end',"Edit this text!"}},
		      {vscroll, right},
		      {hscroll, true},
		      {wrap, none}]),
  ok.

-spec create_menu() -> 'ok'.

create_menu() ->
  gs:menubar(menubar, win, [{bg, ?DEFAULT_BG_COLOR}]),
  create_sub_menus([{mbutt, fmenu, " File",
		     [{" Close    Ctrl-C ",close_menu}]},
		    {mbuttc,cmenu, " Compile ",
		     [{" Compile Module", comp_mod}]},
		    {mbuttp,pmenu, " Profile ",
		     [{" Profile Module", prof_mod}]},
		    {mbutte,emenu, " Edoc", [separator]},
		    {mbutta,amenu, " Analyze ", [separator]},
		    {mbuttb,bmenu, " Benchmark ", [separator]},		    
		    {mbuttj,jmenu, " Jit ", [separator]}]),
  ok.

create_menuitems(Parent, [{Text,Data}|Rest]) ->
  gs:menuitem(Parent, [{bg, ?DEFAULT_BG_COLOR},
		       {fg, {178, 34, 34}},
		       {label, {text, Text}},
		       {data, Data},
		       {underline, 1}
		      ]),
  create_menuitems(Parent, Rest);
create_menuitems(Parent, [separator|Rest]) ->
  gs:menuitem(Parent, [{itemtype, separator}]),
  create_menuitems(Parent, Rest);
create_menuitems(_, []) -> ok.

create_sub_menus([{Parent, Name, Text, Items}|Rest]) ->
  BG = {bg, ?DEFAULT_BG_COLOR},
  FG = {fg, {178, 34, 34}},  % firebrick
  Label = {label, {text, Text}},
  gs:menubutton(Parent, menubar, [BG, FG, Label, {underline, 1}]),
  gs:menu(Name, Parent, [BG, FG]),
  create_menuitems(Name, Items),
  create_sub_menus(Rest);
create_sub_menus([]) -> ok.

create_labels([{Name,Y,Text}|Rest], Xpos) ->    
  gs:label(Name, win, [{width, (?WINDOW_WIDTH - 16) div 3},
		       {height, 20},
		       {x, Xpos + 1 + 3},
		       {y, Y},
		       {bg, ?DEFAULT_BG_COLOR},
		       {fg, ?NORMAL_FG_COLOR},
		       {font, ?HEADER_FONT},
		       {align, w},
		       {label, {text, Text}}
		      ]),
  create_labels(Rest,Xpos);
create_labels([],_) -> ok.

-spec update_code_listbox(#state{}) -> #state{}.

update_code_listbox(State) ->
  Mods = lists:sort(mods()),
  case State#state.win_created of
    false ->
      State;
    true ->
      case Mods =:= State#state.mods of
	true -> State;
	false ->
	  update_text(mods,
		      "Loaded Modules ("++
		      integer_to_list(length(Mods))++")"),
	  catch gs:config(code_listbox, [{data, Mods},
					 {items, Mods},
					 {selection, 0}]),
	  update_module_box(State#state{mods = Mods}, 0, Mods, "")  
      end
  end.

-spec update_fun(#state{}, integer(), [mfa()]) -> #state{}.

update_fun(State, Idx, Data) ->
  case State#state.win_created of
    false ->
      State;
    true ->
      MFA = {M,F,A} = get_selection(Idx, Data, {?MODULE,start,0}),
      update_text(function, "Function: "++mfa_to_string(MFA)),
      case in_native(F, A, native_code(M)) of
	true  -> update_text(nativefun, "Native");
	false -> update_text(nativefun, "Emulated")
      end,
      State
  end.

get_selection(Idx, Data, Default) ->
  try lists:nth(Idx+1, Data) catch _:_ -> Default end.

-spec update_module_box(#state{}, integer(), [atom()], string()) -> #state{}.

update_module_box(State, Idx, Data, _Txt) ->
  case State#state.win_created of
    false ->
      State;
    true ->
      Mod = get_selection(Idx, Data, hipe_tool),
      %% io:format("~w\n", [Mod:module_info()]),      
      Info = Mod:module_info(),
      Funs = lists:usort(funs(Mod)), 
      MFAs = mfas(Mod, Funs),
      ModText = atom_to_list(Mod),
      update_text(mod, "Module:"++ModText),
      update_text(compmod, "Compile\nModule\n"++ModText),
      Options = get_compile(Info),
      update_text(ver, get_version(Options)),
      update_text(time, get_time(Options)),
      NativeCode = native_code(Mod),

      Prof = is_profiled(Mod),
      if Prof -> update_text(prof, "Turn off\nProfiling");
	 true -> update_text(prof, "Profile\n"++ModText)
      end,
 
      Mode = get_mode(Funs, NativeCode),
      
      update_text(native, Mode),
      Items = fun_names(Mod, Funs, NativeCode, Prof),

      Selection = {selection, 0},
      catch gs:config(module_listbox, [{data, MFAs},
				       {items, Items},
				       Selection]),
      ProfData = [mfa_to_string(element(1, X)) ++ " " ++
				integer_to_list(element(2,X))
		  || X <- hipe_profile:res(), element(2, X) > 0],
      catch gs:config(profile_listbox, [{data, ProfData},
					{items, ProfData},
					Selection]),
      get_edoc(Mod),
      update_fun(State#state{mindex = Idx, mod = Mod, funs = Funs}, 0, MFAs)
  end.

update_text(Lab, Text) ->
  catch gs:config(Lab, [{label, {text, Text}}]).

%%---------------------------------------------------------------------
%% @doc Returns a list of all loaded modules. 
%%---------------------------------------------------------------------

-spec mods() -> [atom()].

mods() ->
  [Mod || {Mod,_File} <- code:all_loaded()].

-spec funs(module()) -> [fa()].

funs(Mod) -> 
  Mod:module_info(functions).

-spec native_code(module()) -> [fa_address()].

native_code(Mod) ->
  Mod:module_info(native_addresses).

-spec mfas(atom(), [fa()]) -> [mfa()].

mfas(Mod, Funs) ->
  [{Mod,F,A} || {F,A} <- Funs].

-spec fun_names(atom(), [fa()], [fa_address()], boolean()) -> [string()].

fun_names(M, Funs, NativeCode, Prof) ->
  [atom_to_list(F) ++ "/" ++ integer_to_list(A)
   ++
     (case in_native(F, A, NativeCode) of
	true -> " [native] ";
	false -> ""
      end)
   ++
     if Prof ->
	 (catch integer_to_list(hipe_bifs:call_count_get({M,F,A})));
	true -> ""
     end
   || {F,A} <- Funs].

-spec in_native(atom(), arity(), [fa_address()]) -> boolean().

in_native(F, A, NativeCode) ->
  lists:any(fun({Fun,Arity,_}) ->
		(Fun =:= F andalso Arity =:= A)
	    end,
	    NativeCode).

-spec mfa_to_string(mfa()) -> [char(),...].

mfa_to_string({M,F,A}) ->
  atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A).

get_mode(Funs, NativeCode) ->
  case NativeCode of
    [] -> "Emulated";
    InNative when is_list(InNative) ->
      if length(InNative) =:= length(Funs) ->
	  "Native";
	 true -> "Mixed"
      end
  end.

get_time(Comp) ->
  case lists:keyfind(time, 1, Comp) of
    {_, {Y,Month,D,H,Min,S}} ->
      integer_to_list(Y) ++
	"-" ++ integer_to_list(Month) ++
	"-" ++ integer_to_list(D) ++ " " ++
	integer_to_list(H) ++ ":" ++ integer_to_list(Min) ++
	":"  ++ integer_to_list(S);
    false -> ""
  end.

get_version(Comp) ->
  case lists:keyfind(version, 1, Comp) of
    {_, V} when is_list(V) -> V;
    false -> ""
  end.

get_cwd(Options) ->
  case lists:keyfind(cwd, 1, Options) of
    {_, V} when is_atom(V) -> atom_to_list(V);
    {_, V} -> V; 
    false -> ""
  end.

get_options(Comp) ->
  case lists:keyfind(options, 1, Comp) of
    {_, V} when is_list(V) -> V;
    false -> ""
  end.

get_compile(Info) ->
  case lists:keyfind(compile, 1, Info) of
    {_, O} when is_list(O) -> O;
    false -> []
  end.

-spec is_profiled(atom()) -> boolean().

is_profiled(Mod) ->
  case hipe_bifs:call_count_get({Mod,module_info,0}) of
    false -> false;
    C when is_integer(C) -> true
  end.

-spec compile(#state{}) -> #state{}.

compile(State) ->
  catch gs:config(compmod, [{enable, false}]),
  update_text(compiling, "Compiling..."),
  Parent = self(),
  P = spawn(fun() -> c(Parent, State#state.mod, State#state.options) end),
  State#state{compiling = P}.

-spec c(pid(), atom(), comp_options()) -> 'ok'.

c(Parent, Mod, Options) ->
  Res = hipe:c(Mod, Options),
  Parent ! {compilation_done,Res,self()},
  ok.

get_edoc(Mod) ->
  Info = Mod:module_info(),
  Comp = get_compile(Info), 
  Options = get_options(Comp),
  Dir = get_cwd(Options),
  File = 
    case Dir of
      "" ->  atom_to_list(Mod) ++ ".erl";
      _ -> Dir ++"/" ++ atom_to_list(Mod) ++ ".erl"
    end,
  %% io:format("Get ~s\n", [File]),
  Text = try edoc(File, [{xml_export,xmerl_text}, no_output])
	 catch  _:_ -> "error"
	 end,
  gs:config(edoc, {enable, true}),
  gs:config(edoc, clear),
  gs:config(edoc, {insert, {insert, Text}}),
  gs:config(edoc, {enable, false}),
  ok.

edoc(Name, Opts) ->
  Doc = edoc:get_doc(Name, Opts),
  %% Comments = edoc:read_comments(Name, Opts),
  %% Text = edoc:forms(Forms, Comments, Name, Opts),
  edoc:layout(Doc, Opts),
  ok.
