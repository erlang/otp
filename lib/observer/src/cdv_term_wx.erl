%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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
-module(cdv_term_wx).

-export([get_details/1,
	 detail_pages/0]).

%% Callbacks for cdv_detail_win
get_details({T,Key}) ->
    [{Key,Term}] = ets:lookup(T,Key),
    {ok,{"Expanded Term", Term, []}}.

detail_pages() ->
    [{"Term", fun init_term_page/2}].

init_term_page(ParentWin, Term) ->
    cdv_multi_panel:start_link(
      ParentWin,
      [{"Format \~p",cdv_html_page,format_term_fun("~p",Term)},
       {"Format \~tp",cdv_html_page,format_term_fun("~tp",Term)},
       {"Format \~w",cdv_html_page,format_term_fun("~w",Term)},
       {"Format \~s",cdv_html_page,format_term_fun("~s",expand(Term))},
       {"Format \~ts",cdv_html_page,format_term_fun("~ts",expand(Term))}]).

format_term_fun(Format,Term) ->
    fun() ->
	    try io_lib:format(Format,[Term]) of
		Str -> plain_html(Str)
	    catch error:badarg ->
		    Warning = "This term can not be formatted with " ++ Format,
		    crashdump_viewer_html:warning(Warning)
	    end
    end.

plain_html(Text) ->
    crashdump_viewer_html:plain_page(Text).

expand(['#CDVBin',Offset,Size,Pos]) ->
    {ok,Bin} = crashdump_viewer:expand_binary({Offset,Size,Pos}),
    Bin;
expand([H|T]) ->
    case expand(T) of
	ET when is_list(ET) ->
	    [expand(H)|ET];
        ET -> % The tail is an expanded binary - cannot append with |
	    [expand(H),ET]
    end;
expand(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(expand(tuple_to_list(Tuple)));
expand(Term) ->
    Term.
