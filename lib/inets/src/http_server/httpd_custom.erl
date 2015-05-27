%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2015-2015. All Rights Reserved.
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
%%
-module(httpd_custom).

-export([response_header/1, request_header/1]).
-export([customize_headers/3]).

-include_lib("inets/src/inets_app/inets_internal.hrl").

response_header(Header) -> 
    {true, httpify(Header)}.
request_header(Header) -> 
    {true, Header}.

customize_headers(?MODULE, Function, Arg) ->
    ?MODULE:Function(Arg);
customize_headers(Module, Function, Arg) ->
    try Module:Function(Arg) of
	{true, Value} ->
	    ?MODULE:Function(Value);
	false ->
	    false
    catch
	_:_ ->
	    ?MODULE:Function(Arg)
    end.

httpify({Key0, Value}) ->
    %% make sure first letter is capital (defacto standard)
    Words1 = string:tokens(Key0, "-"),
    Words2 = upify(Words1, []),
    Key    = new_key(Words2),
    Key ++ ": " ++ Value ++ ?CRLF .

new_key([]) ->
    "";
new_key([W]) ->
    W;
new_key([W1,W2]) ->
    W1 ++ "-" ++ W2;
new_key([W|R]) ->
    W ++ "-" ++ new_key(R).
    
upify([], Acc) ->
    lists:reverse(Acc);
upify([Key|Rest], Acc) ->
    upify(Rest, [upify2(Key)|Acc]).

upify2([C|Rest]) when (C >= $a) andalso (C =< $z) ->
    [C-($a-$A)|Rest];
upify2(Str) ->
    Str.
