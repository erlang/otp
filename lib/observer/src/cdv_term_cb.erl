%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2018. All Rights Reserved.
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
-module(cdv_term_cb).

-export([get_details/2,
	 detail_pages/0]).

%% Callbacks for cdv_detail_wx
get_details({Type, {T,Key}}, _) ->
    [{Key,Term}] = ets:lookup(T,Key),
    {ok,{"Expanded Term", {Type,[Term, T]}, []}}.

detail_pages() ->
    [{"Term", fun init_term_page/2}].

init_term_page(ParentWin, {Type, [Term, Tab]}) ->
    observer_lib:report_progress({ok,"Expanding term"}),
    observer_lib:report_progress({ok,start_pulse}),
    Expanded = expand(Term, true),
    BinSaved = expand(Term, Tab),
    observer_lib:report_progress({ok,stop_pulse}),
    Cs = observer_lib:colors(ParentWin),
    cdv_multi_wx:start_link(
      ParentWin,
      [{"Format \~p",cdv_html_wx,{Type, format_term_fun("~p",BinSaved,Tab,Cs)}},
       {"Format \~tp",cdv_html_wx,{Type,format_term_fun("~tp",BinSaved,Tab,Cs)}},
       {"Format \~w",cdv_html_wx,{Type,format_term_fun("~w",BinSaved,Tab,Cs)}},
       {"Format \~tw",cdv_html_wx,{Type,format_term_fun("~tw",BinSaved,Tab,Cs)}},
       {"Format \~s",cdv_html_wx,{Type,format_term_fun("~s",Expanded,Tab,Cs)}},
       {"Format \~ts",cdv_html_wx,{Type,format_term_fun("~ts",Expanded,Tab,Cs)}}]).

format_term_fun(Format,Term,Tab,Cs) ->
    fun() ->
            observer_lib:report_progress({ok,"Formatting term"}),
            observer_lib:report_progress({ok,start_pulse}),
	    try io_lib:format(Format,[Term]) of
		Str -> {expand, plain_html(Str,Cs), Tab}
	    catch error:badarg ->
		    Warning = "This term cannot be formatted with " ++ Format,
		    observer_html_lib:warning(Warning,Cs)
            after
                    observer_lib:report_progress({ok,stop_pulse})
	    end
    end.

plain_html(Text,Cs) ->
    observer_html_lib:plain_page(Text,Cs).

expand(['#CDVBin',Offset,Size,Pos], true) ->
    {ok,Bin} = crashdump_viewer:expand_binary({Offset,Size,Pos}),
    Bin;
expand(Bin, Tab) when is_binary(Bin), not is_boolean(Tab) ->
    observer_lib:make_obsbin(Bin, Tab);
expand([H|T], Expand) ->
    case expand(T, Expand) of
	ET when is_list(ET) ->
	    [expand(H, Expand)|ET];
        ET -> % The tail is an expanded binary - cannot append with |
	    [expand(H, Expand),ET]
    end;
expand(Tuple, Expand) when is_tuple(Tuple) ->
    list_to_tuple(expand(tuple_to_list(Tuple), Expand));
expand(Term, _) ->
    Term.
