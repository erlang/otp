%%----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : orber_web_server.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module(orber_web_server).

-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([terminate/2,code_change/3]).
-export([start/0,stop/0,start_link/0]).

-export([config_data/0, menu/2, configure/2, info/2, nameservice/2, 
	 default_selection/2, ifr_select/2, ifr_data/2, create/2,
	 delete_ctx/2, add_ctx/2, delete_obj/2, flash_msg/2]).

%%----------------------------------------------------------------------
%%-------------- Defines & Includes ------------------------------------
%%----------------------------------------------------------------------
-define(HTML_HEADER, 
	"Cache-Control:no-cache\r\nPragma:no-cache\r\nExpires:Thu, 01 Dec 1994 16:00:00 GMT\r\nContent-type: text/html\r\n\r\n<HTML BGCOLOR=\"#FFFFFF\">\n<HEAD>\n<TITLE>Orber O&D</TITLE>\n</HEAD>\n").
         

-define(HTML_END, "</BODY></HTML>").

-define(DEBUG_LEVEL, 5).

-record(state, {ts}).
-include("ifr_objects.hrl").

%%----------------------------------------------------------------------
%%-------------- External API ------------------------------------------
%%----------------------------------------------------------------------
%% Function   : start/start_link/stop
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
start_link()->
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).
start()->
    gen_server:start({local,?MODULE},?MODULE,[],[]).
stop()->
    gen_server:call(?MODULE,stop,1000).

%%----------------------------------------------------------------------
%% Function   : config_data
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
config_data()->
    {orber,[{web_data,{"OrberWeb","/orber/main_frame.html"}},
	    {alias,{"/orber", code:priv_dir(orber)}},
	    {start,{child,{{local,?MODULE},{?MODULE,start_link,[]},
			   permanent,100,worker,[?MODULE]}}},
	    {alias,{erl_alias,"/orber_erl",[orber_web_server]}}
	   ]}.


menu(Env,Input) ->
    Args = httpd:parse_query(Input),
    [?HTML_HEADER,  gen_server:call(?MODULE, {menu, Env, Args}), ?HTML_END].

configure(Env,Input) ->
    Args = httpd:parse_query(Input),
    [?HTML_HEADER, gen_server:call(?MODULE, {configure, Env, Args}), ?HTML_END].

nameservice(Env,Input) ->
    Args = httpd:parse_query(Input),
    [?HTML_HEADER, gen_server:call(?MODULE, {nameservice, Env, Args}), ?HTML_END].

info(Env,Input) ->
    Args = httpd:parse_query(Input),
    [?HTML_HEADER, gen_server:call(?MODULE, {info, Env, Args}), ?HTML_END].

default_selection(Env,Input) ->
    Args = httpd:parse_query(Input),
    [?HTML_HEADER, gen_server:call(?MODULE, {default_selection, Env, Args}), ?HTML_END].

flash_msg(Env, Input) ->
    Args = httpd:parse_query(Input),
    [?HTML_HEADER, gen_server:call(?MODULE, {nameservice, Env, Args}), ?HTML_END].
    
ifr_select(Env, Input) ->
    Args = httpd:parse_query(Input),
    [?HTML_HEADER, gen_server:call(?MODULE, {ifr_select, Env, Args}), ?HTML_END].

ifr_data(Env, Input) ->
    Args = httpd:parse_query(Input),
    [?HTML_HEADER, gen_server:call(?MODULE, {ifr_data, Env, Args}), ?HTML_END].

create(Env, Input) ->
    Args = httpd:parse_query(Input),
    [?HTML_HEADER, gen_server:call(?MODULE, {create, Env, Args}), ?HTML_END].

delete_ctx(Env, Input) ->
    Args = httpd:parse_query(Input),
    [?HTML_HEADER, gen_server:call(?MODULE, {delete_ctx, Env, Args}), ?HTML_END].

add_ctx(Env, Input) ->
    Args = httpd:parse_query(Input),
    [?HTML_HEADER, gen_server:call(?MODULE, {add_ctx, Env, Args}), ?HTML_END].

delete_obj(Env, Input) ->
    Args = httpd:parse_query(Input),
    [?HTML_HEADER, gen_server:call(?MODULE, {delete_obj, Env, Args}), ?HTML_END].

%%----------------------------------------------------------------------
%%-------------- Callback Functions ------------------------------------
%%----------------------------------------------------------------------
%% Function   : MISC gen_server specific callback functions
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
init(_Arg)->
    {M, S, U} = now(),
    TS = M*1000000000000 + S*1000000 + U,
    {ok, #state{ts = TS}}.

terminate(_,_State)->
    ok.

handle_cast(_,State)->
    {noreply,State}.

handle_info(_,State)->
    {noreply,State}.

code_change(_Old_vsn,State,_Extra)->
    {ok,State}.

%%----------------------------------------------------------------------
%% Function   : handle_call
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
handle_call({Function, Env, Args}, _From, State)->
    case catch orber_web:Function(Env, Args) of
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_web:~p(~p);~nEXIT: ~p", 
		      [?LINE, Function, Args, R], ?DEBUG_LEVEL),
	    {reply, "<BODY BGCOLOR=\"#FFFFFF\">Internal Error", State};
	{'EXIT', R1, R2} ->
	    orber:dbg("[~p] orber_web:~p(~p);~nEXIT: ~p~n~p", 
		      [?LINE, Function, Args, R1, R2], ?DEBUG_LEVEL),
	    {reply, "<BODY BGCOLOR=\"#FFFFFF\">Internal Error", State};
	{badrpc, Why} ->
	    orber:dbg("[~p] orber_web:~p(~p);~nbadrpc: ~p", 
		      [?LINE, Function, Args, Why], ?DEBUG_LEVEL),
	    {reply, "<BODY BGCOLOR=\"#FFFFFF\">Internal Error", State};
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_web:~p(~p);~nEXCEPTION: ~p", 
		      [?LINE, Function, Args, E], ?DEBUG_LEVEL),
	    {reply, "<BODY BGCOLOR=\"#FFFFFF\">Internal Error", State};
	{error, Data} ->
	    orber:dbg("[~p] orber_web:~p(~p); ~nReason: ~p", 
		      [?LINE, Function, Args, Data], ?DEBUG_LEVEL),
	    {reply, Data, State};
	Reply ->
	    {reply, Reply, State}
    end;
handle_call(stop, _From, State)->
    {stop, normal, ok, State};
handle_call(What, _From, State)->
    orber:dbg("[~p] orber_web_server:handle_call(~p);", 
	      [?LINE, What], ?DEBUG_LEVEL),
    {reply, "<BODY BGCOLOR=\"#FFFFFF\"><FONT SIZE=6>Unknown Request</FONT>", State}.

%%----------------------------------------------------------------------
%%                           END OF MODULE
%%----------------------------------------------------------------------
