%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
    Expanded = expand(Term, true),
    BinSaved = expand(Term, Tab),
    cdv_multi_wx:start_link(
      ParentWin,
      [{"Format \~p",cdv_html_wx,{Type, format_term_fun("~p",BinSaved,Tab)}},
       {"Format \~tp",cdv_html_wx,{Type,format_term_fun("~tp",BinSaved,Tab)}},
       {"Format \~w",cdv_html_wx,{Type,format_term_fun("~w",BinSaved,Tab)}},
       {"Format \~s",cdv_html_wx,{Type,format_term_fun("~s",Expanded,Tab)}},
       {"Format \~ts",cdv_html_wx,{Type,format_term_fun("~ts",Expanded,Tab)}}]).

format_term_fun(Format,Term,Tab) ->
    fun() ->
	    try io_lib:format(Format,[Term]) of
		Str -> {expand, plain_html(Str), Tab}
	    catch error:badarg ->
		    Warning = "This term can not be formatted with " ++ Format,
		    observer_html_lib:warning(Warning)
	    end
    end.

plain_html(Text) ->
    observer_html_lib:plain_page(Text).

expand(['#CDVBin',Offset,Size,Pos], true) ->
    {ok,Bin} = crashdump_viewer:expand_binary({Offset,Size,Pos}),
    Bin;
expand(Bin, Tab) when is_binary(Bin), not is_boolean(Tab) ->
    Size = byte_size(Bin),
    PrevSize = min(Size, 10) * 8,
    <<Preview:PrevSize, _/binary>> = Bin,
    Hash = erlang:phash2(Bin),
    Key = {Preview, Size, Hash},
    ets:insert(Tab, {Key,Bin}),
    ['#OBSBin',Preview,Size,Hash];
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
