%%%-------------------------------------------------------------------
%%% File    : ttb_autostart.erl
%%% Author  : Bartłomiej Puzoń <bartlomiej.puzon@erlang-solutions.com>
%%% Description : This supervisor is used to resume ttb tracing
%%% Users are able to provide custom restart modules for *_config, as 
%%% file:write/read/delete may not be possible on diskless nodes.
%%%
%%% Created : 31 Jul 2010 by <bartlomiej.puzon@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(ttb_autostart).

-behaviour(gen_server).

%% API
-export([start_link/0,
         read_config/0,
         write_config/1,
         delete_config/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEF_AUTOSTART_MODULE, ?MODULE).
-define(AUTOSTART_FILENAME, "ttb_autostart.bin").

start_link() ->
    gen_server:start_link(?MODULE, no_args, []).

delete_config() ->
    file:delete(?AUTOSTART_FILENAME).

read_config() ->
    case file:read_file(?AUTOSTART_FILENAME) of
        {ok, Data} -> {ok, binary_to_term(Data)};
        Error      -> Error
    end.

write_config(Data) ->
    file:write_file(?AUTOSTART_FILENAME, term_to_binary(Data)).
        
init(no_args) -> 
    case application:get_env(runtime_tools, ttb_autostart_module) of
        {ok, _} ->  ok;
        undefined -> application:set_env(runtime_tools, ttb_autostart_module, ?DEF_AUTOSTART_MODULE)
    end,
    observer_backend:ttb_resume_trace(),
    %%As the process is not needed any more, it will shut itself down
    {ok, no_args, 10000}.

handle_call(_,_,_)       -> {noreply, no_args}.
handle_cast(_,_)         -> {noreply, no_args}.
handle_info(timeout,_)   -> {stop, normal, no_args}.
terminate(_,_)           -> ok.
code_change(_,_,_)       -> {ok, no_args}.

