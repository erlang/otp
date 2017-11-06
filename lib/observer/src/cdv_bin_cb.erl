%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2017. All Rights Reserved.
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
-module(cdv_bin_cb).

-export([get_details/2,
	 detail_pages/0]).

%% Callbacks for cdv_detail_wx
get_details({Type, {T,Key}}, _) ->
    [{Key,Term}] = ets:lookup(T,Key),
    {ok,{"Expanded Binary", {Type, Term}, []}};
get_details({cdv, Id}, _) ->
    {ok,Bin} = crashdump_viewer:expand_binary(Id),
    {ok,{"Expanded Binary", {cvd, Bin}, []}}.

detail_pages() ->
    [{"Binary", fun init_bin_page/2}].

init_bin_page(Parent,{Type,Bin}) ->
    cdv_multi_wx:start_link(
      Parent,
      [{"Format \~p",cdv_html_wx,{Type,format_bin_fun("~p",Bin)}},
       {"Format \~tp",cdv_html_wx,{Type,format_bin_fun("~tp",Bin)}},
       {"Format \~w",cdv_html_wx,{Type,format_bin_fun("~w",Bin)}},
       {"Format \~tw",cdv_html_wx,{Type,format_bin_fun("~tw",Bin)}},
       {"Format \~s",cdv_html_wx,{Type,format_bin_fun("~s",Bin)}},
       {"Format \~ts",cdv_html_wx,{Type,format_bin_fun("~ts",Bin)}},
       {"Hex",cdv_html_wx,{Type,hex_binary_fun(Bin)}},
       {"Term",cdv_html_wx,{Type,binary_to_term_fun(Bin)}}]).

format_bin_fun(Format,Bin) ->
    fun() ->
	    try io_lib:format(Format,[Bin]) of
		Str -> plain_html(lists:flatten(Str))
	    catch error:badarg ->
		    Warning = "This binary can not be formatted with " ++ Format,
		    observer_html_lib:warning(Warning)
	    end
    end.

binary_to_term_fun(Bin) ->
    fun() ->
	    try binary_to_term(Bin) of
		Term -> plain_html(io_lib:format("~tp",[Term]))
	    catch error:badarg ->
		    Warning = "This binary can not be converted to an Erlang term",
		    observer_html_lib:warning(Warning)
	    end
    end.

-define(line_break,25).
hex_binary_fun(Bin) ->
    fun() ->
	    S = "<<" ++ format_hex(Bin,?line_break) ++ ">>",
	    plain_html(io_lib:format("~s",[S]))
    end.

format_hex(<<>>,_) ->
    [];
format_hex(<<B1:4,B2:4>>,_) ->
    [integer_to_list(B1,16),integer_to_list(B2,16)];
format_hex(<<B1:4,B2:4,Bin/binary>>,0) ->
    [integer_to_list(B1,16),integer_to_list(B2,16),$,,$\n,$\s,$\s
     | format_hex(Bin,?line_break)];
format_hex(<<B1:4,B2:4,Bin/binary>>,N) ->
    [integer_to_list(B1,16),integer_to_list(B2,16),$,
     | format_hex(Bin,N-1)].

plain_html(Text) ->
    observer_html_lib:plain_page(Text).
