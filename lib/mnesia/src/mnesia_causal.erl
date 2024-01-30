-module(mnesia_causal).

-export([start/0, get_ts/0, send_msg/0, rcv_msg/3, rcv_msg/4, compare_vclock/2,
         vclock_leq/2, deliver_one/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([new_clock/0, bot/0, is_bot/1]).
-export([get_buffered/0]). % for testing

-export_type([vclock/0]).

-include("mnesia.hrl").

-import(mnesia_lib, [important/2, dbg_out/2, verbose/2, warning/2]).

-behaviour(gen_server).

-type ord() :: lt | eq | gt | cc.
-type lc() :: integer().
-type vclock() :: #{node() => lc()}.
-type msg() :: #commit{}.

-record(state, {send_seq :: integer(), delivered :: vclock(), buffer :: [mmsg()]}).
-record(mmsg,
        {tid :: pid(), tab :: mnesia:table(), msg :: msg(), from :: pid() | undefined}).

-type mmsg() :: #mmsg{}.
-type state() :: #state{}.

%% Helper

-spec bot() -> vclock().
bot() ->
    #{}.

-spec is_bot(vclock()) -> boolean().
is_bot(Ts) when is_map(Ts) andalso map_size(Ts) == 0 ->
    true;
is_bot(Ts) when is_map(Ts) ->
    false;
is_bot(_) ->
    error({badarg, bot}).

%% public API

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec get_ts() -> vclock().
get_ts() ->
    gen_server:call(?MODULE, get_ts).

-spec send_msg() -> {node(), vclock()}.
send_msg() ->
    gen_server:call(?MODULE, send_msg).

-spec get_buffered() -> [mmsg()].
get_buffered() ->
    gen_server:call(?MODULE, get_buf).

%% @doc receive an event from another node
%% @return {vclock(), [event()]} the new vector clock and events ready to
%% be delivered
%% @end
-spec rcv_msg(pid(), #commit{}, mnesia:table()) -> [{pid(), #commit{}, mnesia:table()}].
rcv_msg(Tid, Commit, Tab) ->
    MMsgs =
        gen_server:call(?MODULE,
                        {receive_msg,
                         #mmsg{tid = Tid,
                               msg = Commit,
                               tab = Tab}}),
    [{M#mmsg.tid, M#mmsg.msg, M#mmsg.tab} || M <- MMsgs].

-spec rcv_msg(pid(), #commit{}, mnesia:table(), pid()) ->
                 [{pid(), #commit{}, mnesia:table(), pid()}].
rcv_msg(Tid, Commit, Tab, From) ->
    MMsgs =
        gen_server:call(?MODULE,
                        {receive_msg,
                         #mmsg{tid = Tid,
                               msg = Commit,
                               tab = Tab,
                               from = From}}),
    [{M#mmsg.tid, M#mmsg.msg, M#mmsg.tab, M#mmsg.from} || M <- MMsgs].

-spec deliver_one(#commit{}) -> ok.
deliver_one(#commit{sender = Sender}) ->
    gen_server:call(?MODULE, {deliver_one, Sender}).

%% ====================gen_server callbacks====================
init([Nodes]) ->
    {ok,
     #state{send_seq = 0,
            delivered = new_clock(Nodes),
            buffer = []}};
init(_Args) ->
    {ok,
     #state{send_seq = 0,
            delivered = new_clock(),
            buffer = []}}.

handle_call(send_msg, _From, State = #state{delivered = Delivered, send_seq = SendSeq}) ->
    Deps = Delivered#{node() := SendSeq + 1},
    {reply, {node(), Deps}, State#state{send_seq = SendSeq + 1}};
handle_call({deliver_one, Sender}, _From, State = #state{delivered = Delivered}) ->
    NewDelivered = increment(Sender, Delivered),
    {reply, ok, State#state{delivered = NewDelivered}};
handle_call({receive_msg, MM = #mmsg{msg = #commit{}}}, _From, State = #state{}) ->
    {NewState, Deliverable} = find_deliverable(MM, State),
    {reply, Deliverable, NewState};
handle_call(get_buf, _From, #state{buffer = Buffer} = State) ->
    {reply, Buffer, State};
handle_call(get_ts, _From, #state{delivered = D} = State) ->
    {reply, D, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    warning("unhandled message ~p~n", [Msg]),
    {noreply, State}.

%%% internal functions

%% @doc
%% update the delivered vector clock and buffer
%% @returns {list(), [event()]} the new buffer and events ready to be delivered
%% where we guarantee that if the input message is deliverable then it is at the
%% tail of the list
%% @end
-spec find_deliverable(mmsg(), state()) -> {state(), [mmsg()]}.
find_deliverable(MM = #mmsg{msg = #commit{ts = Deps, sender = Sender}},
                 State = #state{delivered = Delivered, buffer = Buffer}) ->
    case msg_deliverable(Deps, Delivered, Sender) of
        true ->
            dbg_out("input message ~p deliverable~n", [MM]),
            NewDelivered = increment(Sender, Delivered),
            do_find_deliverable(State#state{delivered = NewDelivered, buffer = Buffer}, [MM]);
        false ->
            dbg_out("input message ~p not deliverable~n", [MM]),
            {State, []}
    end.

-spec do_find_deliverable(state(), [mmsg()]) -> {state(), [mmsg()]}.
do_find_deliverable(State = #state{delivered = Delivered, buffer = Buff}, Deliverable) ->
    {Dev, NDev} =
        lists:partition(fun(#mmsg{msg = #commit{ts = Deps, sender = Sender}}) ->
                           msg_deliverable(Deps, Delivered, Sender)
                        end,
                        Buff),
    case Dev of
        [] ->
            {State, Deliverable};
        Dev when length(Dev) > 0 ->
            State2 =
                lists:foldl(fun(#mmsg{msg = #commit{sender = Sender}}, StateIn) ->
                               Dev2 = increment(Sender, StateIn#state.delivered),
                               StateIn#state{delivered = Dev2}
                            end,
                            State,
                            Dev),
            do_find_deliverable(State2#state{buffer = NDev}, Dev ++ Deliverable)
    end.

%% @doc Deps is the vector clock of the event
%% Delivered is the local vector clock
%% @end
-spec msg_deliverable(vclock(), vclock(), node()) -> boolean().
msg_deliverable(Deps, Delivered, Sender) ->
    OtherDeps = maps:remove(Sender, Deps),
    OtherDelivered = maps:remove(Sender, Delivered),
    maps:get(Sender, Deps) == maps:get(Sender, Delivered) + 1
    andalso vclock_leq(OtherDeps, OtherDelivered).

-spec new_clock() -> vclock().
new_clock() ->
    new_clock([node() | nodes()]).

new_clock(Nodes) ->
    dbg_out("new clock with nodes ~p~n", [[node() | nodes()]]),
    maps:from_keys(Nodes, 0).

-spec increment(node(), vclock()) -> vclock().
increment(Node, C) ->
    C#{Node := maps:get(Node, C, 0) + 1}.

%% @doc convenience function for comparing two vector clocks, true when VC1 <= VC2
-spec vclock_leq(vclock(), vclock()) -> boolean().
vclock_leq(VC1, VC2) ->
    R = compare_vclock(VC1, VC2),
    R =:= eq orelse R =:= lt.

-spec compare_vclock(vclock(), vclock()) -> ord().
compare_vclock(VC1, VC2) when map_size(VC1) == 0 andalso map_size(VC2) == 0 ->
    eq;
compare_vclock(VC1, _VC2) when map_size(VC1) == 0 ->
    lt;
compare_vclock(_VC1, VC2) when map_size(VC2) == 0 ->
    gt;
compare_vclock(VClock1, VClock2) ->
    AllKeys =
        maps:keys(
            maps:merge(VClock1, VClock2)),
    {Ls, Gs} =
        lists:foldl(fun(Key, {L, G}) ->
                       C1 = maps:get(Key, VClock1, 0),
                       C2 = maps:get(Key, VClock2, 0),
                       if C1 < C2 -> {L + 1, G};
                          C1 > C2 -> {L, G + 1};
                          true -> {L, G}
                       end
                    end,
                    {0, 0},
                    AllKeys),
    if Ls == 0 andalso Gs == 0 ->
           eq;
       Ls == 0 ->
           gt;
       Gs == 0 ->
           lt;
       true ->
           cc
    end.
