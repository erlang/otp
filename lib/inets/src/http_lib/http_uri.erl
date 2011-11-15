%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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
%% RFC 3986
%% 

-module(http_uri).

-export([parse/1, parse/2, 
	 encode/1, decode/1]).


%%%=========================================================================
%%%  API
%%%=========================================================================
parse(AbsURI) ->
    parse(AbsURI, []).

parse(AbsURI, Opts) ->
    case parse_scheme(AbsURI) of
	{error, Reason} ->
	    {error, Reason};
	{Scheme, Rest} ->
	    case (catch parse_uri_rest(Scheme, Rest, Opts)) of
		{UserInfo, Host, Port, Path, Query} ->
		    {ok, {Scheme, UserInfo, Host, Port, Path, Query}};
		_  ->
		    {error, {malformed_url, AbsURI}}
	    end
    end.

encode(URI) ->
    Reserved = sets:from_list([$;, $:, $@, $&, $=, $+, $,, $/, $?,
			       $#, $[, $], $<, $>, $\", ${, $}, $|,
			       $\\, $', $^, $%, $ ]),
    lists:append(lists:map(fun(Char) ->
				   uri_encode(Char, Reserved)
			   end, URI)).

decode([$%,Hex1,Hex2|Rest]) ->
    [hex2dec(Hex1)*16+hex2dec(Hex2)|decode(Rest)];
decode([First|Rest]) ->
    [First|decode(Rest)];
decode([]) ->
    [].

%%%========================================================================
%%% Internal functions
%%%========================================================================
parse_scheme(AbsURI) ->
    case split_uri(AbsURI, ":", {error, no_scheme}, 1, 1) of
	{error, no_scheme} ->
	    {error, no_scheme};
	{StrScheme, Rest} ->
	    case list_to_atom(http_util:to_lower(StrScheme)) of
		Scheme when (Scheme =:= http) orelse (Scheme =:= https) ->
		    {Scheme, Rest};
		Scheme ->
		    {error, {not_supported_scheme, Scheme}}
	    end
    end.

parse_uri_rest(Scheme, "//" ++ URIPart, Opts) ->
    {Authority, PathQuery} =
	case split_uri(URIPart, "/", URIPart, 1, 0) of
	    Split = {_, _} ->
		Split;
	    URIPart ->
		case split_uri(URIPart, "\\?", URIPart, 1, 0) of
		    Split = {_, _} ->
			Split;
		    URIPart ->
			{URIPart,""}
		end
	end,

    {UserInfo, HostPort} = split_uri(Authority, "@", {"", Authority}, 1, 1),
    {Host, Port}         = parse_host_port(Scheme, HostPort, Opts),
    {Path, Query}        = parse_path_query(PathQuery),
    {UserInfo, Host, Port, Path, Query}.


parse_path_query(PathQuery) ->
    {Path, Query} =  split_uri(PathQuery, "\\?", {PathQuery, ""}, 1, 0),
    {path(Path), Query}.

parse_host_port(Scheme,"[" ++ HostPort, Opts) -> %ipv6
    DefaultPort = default_port(Scheme),
    {Host, ColonPort} = split_uri(HostPort, "\\]", {HostPort, ""}, 1, 1),
    Host2 = maybe_ipv6_host_with_brackets(Host, Opts),
    {_, Port} = split_uri(ColonPort, ":", {"", DefaultPort}, 0, 1),
    {Host2, int_port(Port)};

parse_host_port(Scheme, HostPort, _Opts) ->
    DefaultPort = default_port(Scheme),
    {Host, Port} = split_uri(HostPort, ":", {HostPort, DefaultPort}, 1, 1),
    {Host, int_port(Port)}.

split_uri(UriPart, SplitChar, NoMatchResult, SkipLeft, SkipRight) ->
    case inets_regexp:first_match(UriPart, SplitChar) of
	{match, Match, _} ->
	    {string:substr(UriPart, 1, Match - SkipLeft),
	     string:substr(UriPart, Match + SkipRight, length(UriPart))};
	nomatch ->
	    NoMatchResult
    end.

maybe_ipv6_host_with_brackets(Host, Opts) ->
    case lists:keysearch(ipv6_host_with_brackets, 1, Opts) of
	{value, {ipv6_host_with_brackets, true}} ->
	    "[" ++ Host ++ "]";
	_ ->
	    Host
    end.

default_port(http) ->
    80;
default_port(https) ->
    443.

int_port(Port) when is_integer(Port) ->
    Port;
int_port(Port) when is_list(Port) ->
    list_to_integer(Port).

path("") ->
    "/";
path(Path) ->
    Path.

uri_encode(Char, Reserved) ->
    case sets:is_element(Char, Reserved) of
	true ->
	    [ $% | http_util:integer_to_hexlist(Char)];
	false ->
	    [Char]
    end.

hex2dec(X) when (X>=$0) andalso (X=<$9) -> X-$0;
hex2dec(X) when (X>=$A) andalso (X=<$F) -> X-$A+10;
hex2dec(X) when (X>=$a) andalso (X=<$f) -> X-$a+10.
