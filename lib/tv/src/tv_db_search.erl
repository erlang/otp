%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
%%%*********************************************************************
%%% 
%%%   Description:      Code for the search window.
%%%
%%%*********************************************************************
-module(tv_db_search).



-export([create_window/1,
	 resize_window/1,
	 reset_window/1,
	 destroy_window/1,
	 mark_busy/1,
	 mark_nonbusy/1,
	 get_input_and_search/3,
	 update_search/4,
	 string_to_term/1
	]).




-include("tv_int_def.hrl").
-include("tv_int_msg.hrl").
-include("tv_db_int_def.hrl").




-define(WIN_WIDTH, 445).
-define(SMALL_WIN_HEIGHT, 117).
-define(BIG_WIN_HEIGHT, 335).
-define(FRAME_WIDTH, 429).  % 334
-define(OLD_FRAME_WIDTH, 334). 
-define(FRAME_HEIGHT, 105).
-define(FRAME_XPOS, (10-2)).
-define(FRAME_YPOS, 10).
-define(ENTRY_XPOS, 9).
-define(ENTRY_YPOS, 31).
-define(ENTRY_WIDTH, (?OLD_FRAME_WIDTH-10-2*?ENTRY_XPOS-5)).
-define(LISTBOX_WIDTH, ?WIN_WIDTH-2*?FRAME_XPOS+1).
-define(LISTBOX_HEIGHT, 162).
-define(LISTBOX_XPOS, ?FRAME_XPOS-2).
-define(LISTBOX_YPOS, ?SMALL_WIN_HEIGHT+8).
-define(BTN_WIDTH, 80).
-define(BTN_HEIGHT, 30).
-define(BTN_XPOS, ?OLD_FRAME_WIDTH-6).
-define(BG_COLOUR, {217,217,217}).






create_window(true) ->
    gs:config(win, [raise]);
create_window(false) ->
    gs:window(win, gs:start(), [{width,?WIN_WIDTH},
				{height,?SMALL_WIN_HEIGHT},
				{data,small},
				{bg,?BG_COLOUR},
				{title,"[TV]   Search Object"},
				{destroy,true},
				{configure,true},
				{cursor,arrow}
			       ]),

    F = gs:frame(win, [{width,?FRAME_WIDTH},
		       {height,?FRAME_HEIGHT},
		       {x,?FRAME_XPOS},
		       {y,?FRAME_YPOS},
		       {bw,2},
		       {bg,?BG_COLOUR}
		      ]),

    gs:label(F, [{width,80},
		 {height,25},
		 {x,?ENTRY_XPOS+2},
		 {y,8},
		 {align,w},
		 {bg,?BG_COLOUR},
		 {fg, {0,0,0}},
		 {label, {text,"Search for:"}}
		]),

    gs:entry(entry, F, [{width,?ENTRY_WIDTH},
			{height,30},
			{x,?ENTRY_XPOS},
			{y,?ENTRY_YPOS},
			{insert, {0,"<Search expression>"}},
			{bg, {255,255,255}},
			{fg, {0,0,0}},
			{cursor,text},
			{justify,left},
			{keypress,true},
			{setfocus,true}
		       ]),

    Group = list_to_atom("expr" ++ pid_to_list(self())),
    RadioWidth = round(?ENTRY_WIDTH / 2),
    gs:radiobutton(expr_term, F, [{width,RadioWidth - 45},
				  {height,25},
				  {x,?ENTRY_XPOS},
				  {y,?ENTRY_YPOS+40},
				  {group,Group},
				  {align, c},
				  {label,{text,"as term"}},
				  {select,true}
				 ]),
    gs:radiobutton(expr_regexp, F, [{width,RadioWidth + 45},
				    {height,25},
				    {x,?ENTRY_XPOS+RadioWidth-20-26},
				    {y,?ENTRY_YPOS+40},
				    {group,Group},
				    {align,c},
				    {label,{text,"as regular expression"}}
				   ]),
        
    gs:button(search, F, [{width,?BTN_WIDTH},
			  {height,?BTN_HEIGHT},
			  {x,?BTN_XPOS},
			  {y,11},
			  {label, {text,"Search"}},
			  {bg,?BG_COLOUR},
			  {fg, {0,0,0}}
			 ]),
    gs:button(cancel, F, [{width,?BTN_WIDTH},
			  {height,?BTN_HEIGHT},
			  {x,?BTN_XPOS},
			  {y,?BTN_HEIGHT+11+10},
			  {label, {text,"Cancel"}},
			  {data,cancel},
			  {bg,?BG_COLOUR},
			  {fg, {0,0,0}}
			 ]),
    expand_window(),
    gs:config(entry, [{select, {0,1000}}]),
    gs:config(win, [{map,true}]).
    
			    


resize_window(false) ->
    done;
resize_window(true) ->
    gs:config(win, [{width,?WIN_WIDTH},
		    {height,?BIG_WIN_HEIGHT}
		   ]).
			    


		   
reset_window(false) ->
    done;
reset_window(true) ->
    gs:config(listbox, [clear]),
    gs:config(objects_found, [{label, {text,""}}]).


				

destroy_window(false) ->
    done;
destroy_window(true) ->
    gs:destroy(win).



mark_busy(false) ->
    done;
mark_busy(true) ->
    gs:config(win, [{cursor,busy}]),
    gs:config(entry, [{cursor,busy}]).
    



mark_nonbusy(false) ->
    done;
mark_nonbusy(true) ->
    gs:config(win, [{cursor,arrow}]),
    gs:config(entry, [{cursor,text}]).

	    


get_input_and_search(DbList, IsRegExp, ListAsStr) ->
    get_input_and_search(DbList, IsRegExp, true, ListAsStr).
    



get_input_and_search(DbList, IsRegExp, Notify, ListAsStr) ->
    Str = get_entry_text(),
    StrConvRes = case IsRegExp of 
		     true ->
			 string_to_regexp(Str);
		     false ->
			 string_to_term(Str)
		 end,

    case StrConvRes of
	{ok, TermOrRE} ->
	    search(IsRegExp, TermOrRE, DbList, ListAsStr);
	{error, {_Reason, Msg}} when Notify ->
	    gs:config(win, [beep]),
	    tv_utils:notify(win, "TV Notification", Msg);
	{error, {_Reason, _Msg}} ->
	    done
    end.



update_search(false, _DbList, _IsRegExp, _ListAsStr) ->
    done;
update_search(true, DbList, true, ListAsStr) ->
    get_input_and_search(DbList, false, false, ListAsStr);
update_search(true, DbList, false, ListAsStr) ->
    get_input_and_search(DbList, true, false, ListAsStr).
    
	    

get_entry_text() ->
    gs:read(entry,text).



string_to_regexp(Str) ->
    case regexp:parse(Str) of
	{ok, RegExp} ->
	    {ok, RegExp};
	_Error ->
	    case get(error_msg_mode) of
		normal ->
		    {error, {not_a_regexp, "Please enter a regular expression!"}};
		haiku ->
		    {error, {not_a_regexp, ["Being incorrect",
					    "The regular expression",
					    "Must now be retyped."]}}
	    end
    end.



string_to_term(Str) ->
    case catch erl_scan:string(Str ++ ". ") of
	{ok, ScannedStr, _No} ->
	    case erl_parse:parse_term(ScannedStr) of
		{ok, Term} ->
		    {ok, Term};
		_Other ->
		       %% May be a PID, have to check this, since erl_scan
		       %% currently cannot handle this case...  :-(
		    case catch list_to_pid(Str) of
			Pid when is_pid(Pid) ->
			    {ok, Pid};
			_Error ->
			    case get(error_msg_mode) of
				normal ->
				    {error, {not_a_term, "Please enter a valid term!"}};
				haiku ->
				    {error, {not_a_term, ["Aborted effort.",
							  "Reflect, repent and retype:",
							  "Enter valid term."]}}
			    end
		    end
	    end;
	_Error ->
	    case get(error_msg_mode) of
		normal ->
		    {error, {not_a_term, "Please enter a valid term!"}};
		haiku ->
		    {error, {not_a_term, ["Aborted effort.",
					  "Reflect, repent and retype:",
					  "Enter valid term."]}}
	    end
    end.
    


search(IsRegExp, SearchValue, DbList, ListAsStr) ->
    gs:config(cancel, [{label, {text,"Stop"}}]),
    mark_busy(true),
    reset_window(true),
    SearchRes = traverse(SearchValue, DbList, 1, length(DbList), [], IsRegExp, ListAsStr),
    gs:config(cancel, [{label, {text,"Cancel"}}]),
    mark_nonbusy(true),
    SearchRes.





expand_window() ->
    gs:listbox(listbox, win, [{width,?LISTBOX_WIDTH},
			      {height,?LISTBOX_HEIGHT},
			      {x,?LISTBOX_XPOS},
			      {y,?LISTBOX_YPOS},
			      {bg, {255,255,255}},
			      {fg, {0,0,0}},
			      {scrollbg,?BG_COLOUR},
			      {scrollfg,?BG_COLOUR},
			      {hscroll,bottom},
			      {vscroll,right},
			      {click,true},
			      {doubleclick,false},
			      {selectmode,single}
			     ]),
    gs:label(objects_found, win, [{width,?LISTBOX_WIDTH},
				  {height,25},
				  {x,?LISTBOX_XPOS},
				  {y,?LISTBOX_YPOS+?LISTBOX_HEIGHT+13},
				  {align,w},
				  {bg,?BG_COLOUR},
				  {fg, {0,0,0}}
				 ]),
    gs:config(win, [{width,?WIN_WIDTH},
		    {height,?BIG_WIN_HEIGHT}
		   ]).
    


    



traverse(Pattern, [Object | T], Row, Length, Acc, IsRegExp, ListAsStr) ->
    SearchRes = 
	case IsRegExp of
	    true ->
		search_for_regexp(Pattern, Object, ListAsStr);
	    false ->
		compare_terms(Pattern, Object)
	end,

    NewAcc 
	= case SearchRes of
	      found ->
		  RowStr    = integer_to_list(Row),
		  LengthStr = integer_to_list(Length),
		  ObjectStr = case ListAsStr of
				  true ->
				      lists:flatten(tv_io_lib:format("~p", [Object]));
				  false ->
				      lists:flatten(tv_io_lib:write(Object))
			      end,
		  
		  gs:config(listbox, 
			    [{add,
			      "  Row " ++ RowStr ++ ":" ++
			      lists:duplicate(length(LengthStr)-length(RowStr), " ") ++ 
			      "   " ++ ObjectStr}
			    ]),
		  gs:config(objects_found, 
			    [{label, 
			      {text,integer_to_list(length(Acc)+1) ++ 
			       " object(s) found"}}
			    ]),
		  [{Row,Object} | Acc];
	      not_found ->
		  Acc
	  end,
    receive 
	{gs,cancel,click,_Data,_Args} ->
	    gs:config(objects_found, 
		      [{label, 
			{text,integer_to_list(gs:read(listbox,size)) ++ 
			 " object(s) found"}}
		      ]),
	    lists:reverse(NewAcc)
    after 
	0 ->
	    traverse(Pattern, T, Row+1, Length, NewAcc, IsRegExp, ListAsStr)
    end;
traverse(_Pattern, [], _N, _Length, Acc, _IsRegExp, _ListAsStr) ->
    gs:config(objects_found, 
	      [{label, 
		{text,integer_to_list(gs:read(listbox,size)) ++ 
		 " object(s) found"}}
	      ]),
    lists:reverse(Acc).




search_for_regexp(Pattern, Elem, ListAsStr) -> 
    ListToSearch = 
	case ListAsStr of
	    true ->
		lists:flatten(tv_io_lib:format("~p", [Elem]));
	    false ->
		lists:flatten(tv_io_lib:write(Elem))
	end,

    case regexp:first_match(ListToSearch, Pattern) of
	{match, _, _} ->
	    found;
	_Other ->
	    not_found
	    %% The code below shall be used instead if it is desired to 
	    %% compare each *element* in the tuples to the regular expression,
	    %% i.e., treat each element as a new line/string.
	    %% The difference is most easily explained through an example:
	    %% If we treat each tuple as a new line/string, the regular expression
	    %% "^{win" will match the string "{win, 1, 2, 3}", but not the string 
	    %% "{1, {win,2}}".
	    %% If we treat each element as a new line/string, the RE "^{win" will match
	    %% both strings above.
    
	    %% SearchList = tuple_to_list(Elem),
	    %% case lists:dropwhile(
	    %%	   fun(H) ->
	    %%		   nomatch == regexp:first_match(lists:flatten(io_lib:write(H)), 
	    %%						 Pattern)
	    %%	   end,
	    %%	   SearchList) of
	    %%	[] ->
	    %%	    not_found;
	    %%	_AnyList ->
	    %%	    found
	    %% end
    end.





compare_terms(Term, Elem) when not is_tuple(Elem), not is_list(Elem), Term =/= Elem ->
    not_found;
compare_terms(Term, Term) ->
       %% Even the case Term = "{}" or "[]"!!!
    found;
compare_terms(Term, Elem) when is_list(Elem) ->
    traverse_list(Term, Elem);
compare_terms(Term, Elem) when is_tuple(Elem) ->
    traverse_tuple(Term, Elem, 1, size(Elem)).



	

traverse_tuple(Pattern, Tuple, N, Stop) when N =< Stop ->
    Elem = element(N,Tuple),
    case compare_terms(Pattern, Elem) of
	found ->
	    found;
	not_found ->
	    traverse_tuple(Pattern, Tuple, N+1, Stop)
    end;
traverse_tuple(_Pattern, _Tuple, N, Stop) when N > Stop ->
    not_found.






traverse_list(Pattern, [H | T]) ->
    case compare_terms(Pattern, H) of
	found ->
	    found;
	not_found ->
	    traverse_list(Pattern, T)
    end;
traverse_list(_Pattern, []) ->
    not_found.

