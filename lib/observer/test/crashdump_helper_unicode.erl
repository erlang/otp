-module(crashdump_helper_unicode).
-behaviour(gen_server).
-export([start/0, init/1, handle_call/3, handle_cast/2]).
-record(state, {s,a,b,lb}).

start() ->
    gen_server:start({local, 'unicode_reg_name_αβ'}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    ets:new('tab_αβ',[set,named_table]),
    Bin = <<"bin αβ"/utf8>>,
    LongBin = <<"long bin αβ - a utf8 binary which can be expanded αβ"/utf8>>,
    {ok, #state{s = "unicode_string_αβ",
                a = 'unicode_atom_αβ',
                b = Bin,
                lb = LongBin}}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.
handle_cast(_Info, State) ->
    {noreply, State}.
