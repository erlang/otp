%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
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

%%% Description: OLD API MODULE - USE httpc INSTEAD

-module(http).

-deprecated({request,         1, next_major_release}).
-deprecated({request,         2, next_major_release}).
-deprecated({request,         4, next_major_release}).
-deprecated({request,         5, next_major_release}).
-deprecated({cancel_request,  1, next_major_release}).
-deprecated({cancel_request,  2, next_major_release}).
-deprecated({set_option,      2, next_major_release}).
-deprecated({set_option,      3, next_major_release}).
-deprecated({set_options,     1, next_major_release}).
-deprecated({set_options,     2, next_major_release}).
-deprecated({verify_cookies,  2, next_major_release}).
-deprecated({verify_cookies,  3, next_major_release}).
-deprecated({cookie_header,   1, next_major_release}).
-deprecated({cookie_header,   2, next_major_release}).
-deprecated({stream_next,     1, next_major_release}).
-deprecated({default_profile, 0, next_major_release}).

%% Deprecated
-export([
	 request/1, request/2, request/4, request/5,
	 cancel_request/1, cancel_request/2,
	 set_option/2, set_option/3,
	 set_options/1, set_options/2,
	 verify_cookies/2, verify_cookies/3, 
	 cookie_header/1, cookie_header/2, 
	 stream_next/1,
	 default_profile/0
	]).


%%%=========================================================================
%%%  API
%%%=========================================================================

%%--------------------------------------------------------------------------
%% request(Url [, Profile]) ->
%% request(Method, Request, HTTPOptions, Options [, Profile])
%%--------------------------------------------------------------------------

request(Url)          -> httpc:request(Url).
request(Url, Profile) -> httpc:request(Url, Profile).

request(Method, Request, HttpOptions, Options) ->
    httpc:request(Method, Request, HttpOptions, Options). 
request(Method, Request, HttpOptions, Options, Profile) ->
    httpc:request(Method, Request, HttpOptions, Options, Profile). 


%%--------------------------------------------------------------------------
%% cancel_request(RequestId [, Profile])
%%-------------------------------------------------------------------------

cancel_request(RequestId) ->
    httpc:cancel_request(RequestId).
cancel_request(RequestId, Profile) ->
    httpc:cancel_request(RequestId, Profile).


%%--------------------------------------------------------------------------
%% set_options(Options [, Profile])
%% set_option(Key, Value [, Profile])
%%-------------------------------------------------------------------------

set_options(Options) ->
    httpc:set_options(Options).
set_options(Options, Profile) ->
    httpc:set_options(Options, Profile).

set_option(Key, Value) ->
    httpc:set_option(Key, Value).
set_option(Key, Value, Profile) ->
    httpc:set_option(Key, Value, Profile).


%%--------------------------------------------------------------------------
%% verify_cookies(SetCookieHeaders, Url [, Profile])
%%-------------------------------------------------------------------------

verify_cookies(SetCookieHeaders, Url) ->
    httpc:store_cookies(SetCookieHeaders, Url).
verify_cookies(SetCookieHeaders, Url, Profile) ->
    httpc:store_cookies(SetCookieHeaders, Url, Profile).


%%--------------------------------------------------------------------------
%% cookie_header(Url [, Profile])
%%-------------------------------------------------------------------------

cookie_header(Url) ->
    httpc:cookie_header(Url).
cookie_header(Url, Profile) ->
    httpc:cookie_header(Url, Profile).


%%--------------------------------------------------------------------------
%% stream_next(Pid)
%%-------------------------------------------------------------------------

stream_next(Pid) ->
    httpc:stream_next(Pid).


%%--------------------------------------------------------------------------
%% default_profile()
%%-------------------------------------------------------------------------

default_profile() ->
    httpc:default_profile().
