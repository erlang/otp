%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%
%%

-module(httpd_poll).
-behaviour(gen_server).


%% External API
-export([start/0, start_appup/2, start/3,stop/0,verbosity/1,poll_time/1]).

%% gen_server exports
-export([init/1, 
	 handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).


-define(default_verbosity,error).
-define(default_poll_time,60000). %% 60 seconds


-record(state,{host = "", port = -1, ptime = -1, tref = none, uris = []}).


%% start/0
%%
%% Description: Start polling HTTPD with default values
%%
start() -> 
    Options = default_options(otp), 
    start("gandalf", 8000, Options).

start_appup(Host, Port) ->
    Options = default_options(top), 
    start(Host, Port, Options).

%% start/3
%%
%% Description: Start polling HTTPD
%%
%% Parameters:
%%              Host        = string()
%%                            Host name of HTTPD
%%              Port        = integer()
%%                            Port number of HTTPD
%%              Options     = [Option]
%%              Option      = {poll_time,integer()} | {verbosity,verbosity()} |
%%                            {log_file,string()}   | {uris,[uri()]}
%%              verbosity() = silence | error | log | debug | trace
%%              uri()       = {string(),string}
%%                            First part is a descriptive string and the second
%%                            part is the actual URI.
%%
start(Host,Port,Options) ->
    gen_server:start({local,httpd_tester},?MODULE,[Host,Port,Options],[]).
    
stop() ->
    gen_server:call(httpd_tester,stop).


default_options(UriDesc) ->
    Verbosity = {verbosity,?default_verbosity},
    Uris      = {uris,uris(UriDesc)},
    PollTime  = {poll_time,?default_poll_time},
    Logging   = {log_file,"httpd_poll.log"},
    [Verbosity, Uris, PollTime, Logging].


options(Options) ->
    options(Options, default_options(otp), []).

options([], Defaults, Options) ->
    Options ++ Defaults;
options([{Key, _Val} = Opt|Opts], Defaults, Options) ->
    options(Opts, lists:keydelete(Key, 1, Defaults), [Opt | Options]).


verbosity(silence) ->
    set_verbosity(silence);
verbosity(error) ->
    set_verbosity(error);
verbosity(log) ->
    set_verbosity(log);
verbosity(debug) ->
    set_verbosity(debug);
verbosity(trace) ->
    set_verbosity(trace).

set_verbosity(Verbosity) ->
    gen_server:cast(httpd_tester,{verbosity,Verbosity}).

poll_time(NewTime) ->
    gen_server:call(httpd_tester,{poll_time,NewTime}).


%% ----------------------------------------------------------------------


init([Host, Port, Options0]) ->
    process_flag(trap_exit,true),
    Options = options(Options0),
    put(verbosity,get_verbosity(Options)),
    log_open(get_log_file(Options)),
    tstart(),
    PollTime = get_poll_time(Options),
    Ref = tcreate(PollTime),
    log("created"),
    {ok,#state{host  = Host,
	       port  = Port,
	       ptime = PollTime,
	       tref  = Ref,
	       uris  = get_uris(Options)}}.

uris(top) ->
    [uri_top_index()];

uris(otp) ->
    [
     uri_top_index(),
     uri_internal_product1(),
     uri_internal_product2(),
     uri_r13b03_test_results(),
     uri_bjorn1(),
     uri_bjorn2()
    ].

uri_top_index() -> 
    {"top page","/"}.

uri_internal_product1() -> 
    {"product internal page (1)","/product/internal/"}.

uri_internal_product2() -> 
    {"product internal page (2)","/product/internal"}.

uri_r13b03_test_results() ->
    {"daily build index page",
     "/product/internal/test/daily/logs.html"}.

uri_bjorn1() ->
    {"bjorns home page (1)","/~bjorn/"}.

uri_bjorn2() ->
    {"bjorns home page (2)","/~bjorn"}.


handle_call(stop, _From, State) ->
    vlog("stop request"),
    {stop, normal, ok, State};

handle_call({poll_time,NewTime}, _From, State) ->
    vlog("set new poll time: ~p",[NewTime]),
    OldTime = State#state.ptime,
    {stop, normal, OldTime, State#state{ptime = NewTime}};

handle_call(Request, _From, State) ->
    vlog("unexpected request(call): ~p",[Request]),
    {reply, ok, State}.


handle_cast({verbosity,Verbosity}, State) ->
    vlog("set (new) verbosity to: ~p",[Verbosity]),
    put(verbosity,Verbosity),
    {noreply, State};

handle_cast(Message, State) ->
    vlog("unexpected message(call): ~p",[Message]),
    {noreply, State}.


handle_info(poll_time,State) ->
    {{Description,Uri},Uris} = get_uri(State#state.uris),
    vlog("poll time for ~s",[Description]),
    do_poll(State#state.host,State#state.port,Uri),
    Ref = tcreate(State#state.ptime),
    {noreply, State#state{tref = Ref, uris = Uris}};

handle_info(Info, State) ->
    vlog("unexpected message(info): ~p",[Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, State) ->
    tcancel(State#state.tref),
    log_close(get(log_file)),
    ok.


get_uri([Uri|Uris]) ->
    {Uri,Uris++[Uri]}.

    
do_poll(Host,Port,Uri) ->
    (catch poll(create(Host,Port),Uri,"200")).

poll({ok,Socket},Uri,ExpStatus) ->
    vtrace("poll -> entry with Socket: ~p",[Socket]),
    put(latest_requested_uri,Uri),
    Req = "GET " ++ Uri ++ " HTTP/1.0\r\n\r\n",
    await_poll_response(send(Socket,Req),Socket,ExpStatus);
poll({error,Reason},_Req,_ExpStatus) ->
    verror("failed creating socket: ~p",[Reason]),
    log("failed creating socket: ~p",[Reason]),
    exit({error,Reason});
poll(O,_Req,_ExpStatus) ->
    verror("unexpected result from socket create: ~p",[O]),
    log("unexpected result from socket create: ~p",[O]),
    exit({unexpected_result,O}).

await_poll_response(ok,Socket,ExpStatusCode) ->
    vtrace("await_poll_response -> awaiting response with status ~s",
	   [ExpStatusCode]),
    receive 
	{tcp_closed,Socket} ->
	    verror("connection closed when awaiting poll response"),
	    log("connection closed when awaiting reply to GET of '~s'",
		[get(latest_requested_uri)]),
	    exit(connection_closed);
	{tcp,Socket,Response} ->
	    vdebug("received response"),
	    validate(ExpStatusCode,Socket,Response)
    after 10000 ->
	    verror("connection timeout waiting for poll response",[]),
	    log("connection timeout waiting for reply to GET of '~s'",
		[get(latest_requested_uri)]),
	    exit(connection_timed_out)
    end;
await_poll_response(Error,_Socket,_ExpStatusCode) ->
    verror("failed sending GET request for '~s' for reason: ~p",
	   [get(latest_requested_uri),Error]),
    log("failed sending GET request for '~s' for reason: ~p",
	[get(latest_requested_uri),Error]),
    exit(Error).
    

validate(ExpStatusCode,Socket,Response) ->
    Sz = sz(Response),
    vtrace("validate -> Entry with ~p bytes response",[Sz]),
    Size = trash_the_rest(Socket,Sz),
    close(Socket),
    case re:split(Response," ", [{return, list}]) of
	["HTTP/1.0",ExpStatusCode|_] ->
	    vlog("response (~p bytes) was ok",[Size]),
	    ok;
	["HTTP/1.0",StatusCode|_] -> 
	    verror("unexpected response status received: ~s => ~s",
		   [StatusCode,status_to_message(StatusCode)]),
	    log("unexpected result to GET of '~s': ~s => ~s",
		[get(latest_requested_uri),StatusCode,
		 status_to_message(StatusCode)]),
	    exit({unexpected_response_code,StatusCode,ExpStatusCode})
    end.


%% ------------------------------------------------------------------

trash_the_rest(Socket,N) ->
    receive
	{tcp, Socket, Trash} ->
	    vtrace("trash_the_rest -> trash ~p bytes",[sz(Trash)]),
	    trash_the_rest(Socket,add(N,sz(Trash)));
	{tcp_closed, Socket} ->
	    vdebug("socket closed after receiving ~p bytes",[N]),
	    N
    after 10000 ->
	    verror("connection timeout waiting for message"),
	    exit(connection_timed_out)
    end.


add(N1, N2) when is_integer(N1) andalso is_integer(N2) ->
    N1 + N2;
add(N1, _N2) when is_integer(N1) ->
    N1;
add(_N1, N2) when is_integer(N2) ->
    N2.

sz(L) when is_list(L) ->
    length(lists:flatten(L));
sz(B) when is_binary(B) ->
    size(B);
sz(O) ->
    {unknown_size,O}.


%% --------------------------------------------------------------
%%
%% Status code to printable string
%%

status_to_message(L) when is_list(L) ->
    case (catch list_to_integer(L)) of
	I when is_integer(I) ->
	    status_to_message(I);
	_ ->
	    io_lib:format("UNKNOWN STATUS CODE: '~p'",[L])
    end;
status_to_message(100) -> "Section 10.1.1: Continue";
status_to_message(101) -> "Section 10.1.2: Switching Protocols";
status_to_message(200) -> "Section 10.2.1: OK";
status_to_message(201) -> "Section 10.2.2: Created";
status_to_message(202) -> "Section 10.2.3: Accepted";
status_to_message(203) -> "Section 10.2.4: Non-Authoritative Information";
status_to_message(204) -> "Section 10.2.5: No Content";
status_to_message(205) -> "Section 10.2.6: Reset Content";
status_to_message(206) -> "Section 10.2.7: Partial Content";
status_to_message(300) -> "Section 10.3.1: Multiple Choices";
status_to_message(301) -> "Section 10.3.2: Moved Permanently";
status_to_message(302) -> "Section 10.3.3: Found";
status_to_message(303) -> "Section 10.3.4: See Other";
status_to_message(304) -> "Section 10.3.5: Not Modified";
status_to_message(305) -> "Section 10.3.6: Use Proxy";
status_to_message(307) -> "Section 10.3.8: Temporary Redirect";
status_to_message(400) -> "Section 10.4.1: Bad Request";
status_to_message(401) -> "Section 10.4.2: Unauthorized";
status_to_message(402) -> "Section 10.4.3: Peyment Required";
status_to_message(403) -> "Section 10.4.4: Forbidden";
status_to_message(404) -> "Section 10.4.5: Not Found";
status_to_message(405) -> "Section 10.4.6: Method Not Allowed";
status_to_message(406) -> "Section 10.4.7: Not Acceptable";
status_to_message(407) -> "Section 10.4.8: Proxy Authentication Required";
status_to_message(408) -> "Section 10.4.9: Request Time-Out";
status_to_message(409) -> "Section 10.4.10: Conflict";
status_to_message(410) -> "Section 10.4.11: Gone";
status_to_message(411) -> "Section 10.4.12: Length Required";
status_to_message(412) -> "Section 10.4.13: Precondition Failed";
status_to_message(413) -> "Section 10.4.14: Request Entity Too Large";
status_to_message(414) -> "Section 10.4.15: Request-URI Too Large";
status_to_message(415) -> "Section 10.4.16: Unsupported Media Type";
status_to_message(416) -> "Section 10.4.17: Requested range not satisfiable";
status_to_message(417) -> "Section 10.4.18: Expectation Failed";
status_to_message(500) -> "Section 10.5.1: Internal Server Error";
status_to_message(501) -> "Section 10.5.2: Not Implemented";
status_to_message(502) -> "Section 10.5.3: Bad Gatteway";
status_to_message(503) -> "Section 10.5.4: Service Unavailable";
status_to_message(504) -> "Section 10.5.5: Gateway Time-out";
status_to_message(505) -> "Section 10.5.6: HTTP Version not supported";
status_to_message(Code) -> io_lib:format("Unknown status code: ~p",[Code]).


%% ----------------------------------------------------------------

create(Host,Port) ->
    vtrace("create -> ~n\tHost: ~s~n\tPort: ~p",[Host,Port]),
    case gen_tcp:connect(Host,Port,[{packet,0},{reuseaddr,true}]) of
	{ok,Socket} ->
	    {ok,Socket};
	{error,{enfile,_}} ->
	    {error,enfile};
	Error ->
	    Error
    end.

close(Socket) ->
    gen_tcp:close(Socket).


send(Socket,Data) ->
    vtrace("send -> send ~p bytes of data",[length(Data)]),
    gen_tcp:send(Socket,Data).

    
%% ----------------------------------------------------------------

tstart() -> 
    timer:start().

tcreate(Time) -> 
    {ok,Ref} = timer:send_after(Time,poll_time),
    Ref.

tcancel(Ref) ->
    timer:cancel(Ref).

%% ----------------------------------------------------------------

log_open(undefined) ->
    ok;
log_open(FileName) ->
    put(log_file,fopen(FileName)).

log_close(undefined) ->
    ok;
log_close(Fd) ->
    fclose(Fd).

log(F) ->
    log(F,[]).

log(F,A) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = local_time(),
    fwrite(get(log_file),
	   "~w.~w.~w ~w.~w.~w " ++ F ++ "~n",
	   [Year,Month,Day,Hour,Min,Sec] ++ A).

%% ----------------------------------------------------------------

fopen(Name) ->
    {ok,Fd} = file:open(Name,[write]),
    Fd.

fclose(Fd) ->
    file:close(Fd).

fwrite(undefined,_F,_A) ->
    ok;
fwrite(Fd,F,A) ->
    io:format(Fd,F,A).

    
%% ----------------------------------------------------------------

get_poll_time(Opts) ->
    get_option(poll_time,Opts,?default_poll_time).

get_log_file(Opts) ->
    get_option(log_file,Opts).

get_uris(Opts) ->
    get_option(uris,Opts,[]).

get_verbosity(Opts) ->
    get_option(verbosity,Opts,?default_verbosity).

get_option(Opt,Opts) ->
    get_option(Opt,Opts,undefined).

get_option(Opt,Opts,Default) ->
    case lists:keysearch(Opt,1,Opts) of
	{value,{Opt,Value}} ->
	    Value;
	false ->
	    Default
    end.

%% ----------------------------------------------------------------

%% sleep(T) -> receive after T -> ok end.

%% ----------------------------------------------------------------

%% vtrace(F)   -> vprint(get(verbosity),trace,F,[]).
vtrace(F,A) -> vprint(get(verbosity),trace,F,A).

vdebug(F)   -> vprint(get(verbosity),debug,F,[]).
vdebug(F,A) -> vprint(get(verbosity),debug,F,A).

vlog(F)     -> vprint(get(verbosity),log,F,[]).
vlog(F,A)   -> vprint(get(verbosity),log,F,A).

verror(F)   -> vprint(get(verbosity),error,F,[]).
verror(F,A) -> vprint(get(verbosity),error,F,A).

vprint(trace, Severity,  F,  A)    -> vprint(Severity,F,A);
vprint(debug, trace,    _F, _A)    -> ok;
vprint(debug, Severity,  F,  A)    -> vprint(Severity,F,A);
vprint(log,   log,       F,  A)    -> vprint(log,F,A);
vprint(log,   error,     F,  A)    -> vprint(log,F,A);
vprint(error, error,     F,  A)    -> vprint(error,F,A);
vprint(_Verbosity,_Severity,_F,_A) -> ok.

vprint(Severity,F,A) -> 
    {{Year,Month,Day},{Hour,Min,Sec}} = local_time(),
    io:format("~w.~w.~w ~w.~w.~w " ++ image_of(Severity) ++ F ++ "~n",
	      [Year,Month,Day,Hour,Min,Sec] ++ A).

image_of(error) -> "ERR: ";
image_of(log)   -> "LOG: "; 
image_of(debug) -> "DBG: ";
image_of(trace) -> "TRC: ".
   
local_time() -> calendar:local_time().


