-module(ddfs_master).
-behaviour(gen_server).

-export([start_link/0]).
-export([get_tags/1, get_tags/3,
         get_nodeinfo/1,
         get_read_nodes/0,
         get_hosted_tags/1,
         gc_blacklist/0, gc_blacklist/1,
         gc_stats/0,
         choose_write_nodes/3,
         new_blob/4, new_blob/5,
         safe_gc_blacklist/0, safe_gc_blacklist/1,
         refresh_tag_cache/0,
         tag_notify/2,
         tag_operation/2, tag_operation/3,
         update_gc_stats/1,
         update_nodes/1
        ]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(WEB_PORT, 8011).

-compile(nowarn_deprecated_type).

-include("common_types.hrl").
-include("gs_util.hrl").
-include("config.hrl").
-include("ddfs.hrl").
-include("ddfs_tag.hrl").
-include("ddfs_gc.hrl").

-type node_info() :: {node(), {non_neg_integer(), non_neg_integer()}}.
-type gc_stats() :: none | gc_run_stats().

-record(state, {tags      = gb_trees:empty() :: gb_trees:tree(),
                tag_cache = false            :: false | gb_sets:set(),
                cache_refresher              :: pid(),

                nodes             = []                :: [node_info()],
                write_blacklist   = []                :: [node()],
                read_blacklist    = []                :: [node()],
                gc_blacklist      = []                :: [node()],
                safe_gc_blacklist = gb_sets:empty()   :: gb_sets:set(),
                gc_stats          = none              :: none | {gc_stats(), erlang:timestamp()}}).
-type state() :: #state{}.
-type replyto() :: {pid(), reference()}.

-export_type([gc_stats/0, node_info/0]).

%% ===================================================================
%% API functions

-spec start_link() -> {ok, pid()}.
start_link() ->
    lager:info("DDFS master starts"),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Server} -> {ok, Server};
        {error, {already_started, Server}} -> {ok, Server}
    end.

-spec tag_operation(term(), tagname()) -> term().
tag_operation(Op, Tag) ->
    gen_server:call(?MODULE, {tag, Op, Tag}).
-spec tag_operation(term(), tagname(), non_neg_integer() | infinity) ->
                           term().
tag_operation(Op, Tag, Timeout) ->
    gen_server:call(?MODULE, {tag, Op, Tag}, Timeout).

-spec tag_notify(term(), tagname()) -> ok.
tag_notify(Op, Tag) ->
    gen_server:cast(?MODULE, {tag_notify, Op, Tag}).

-spec get_nodeinfo(all) -> {ok, [node_info()]}.
get_nodeinfo(all) ->
    gen_server:call(?MODULE, {get_nodeinfo, all}).

-spec get_read_nodes() -> {ok, [node()], non_neg_integer()} | {error, term()}.
get_read_nodes() ->
    gen_server:call(?MODULE, get_read_nodes, infinity).

-spec gc_blacklist() -> {ok, [node()]}.
gc_blacklist() ->
    gen_server:call(?MODULE, gc_blacklist).

-spec gc_blacklist([node()]) -> ok.
gc_blacklist(Nodes) ->
    gen_server:cast(?MODULE, {gc_blacklist, Nodes}).

-spec gc_stats() -> {ok, none | {gc_stats(), erlang:timestamp()}} | {error, term()}.
gc_stats() ->
    gen_server:call(?MODULE, gc_stats).

-spec get_hosted_tags(host()) -> {ok, [tagname()]} | {error, term()}.
get_hosted_tags(Host) ->
    gen_server:call(?MODULE, {get_hosted_tags, Host}).

-spec choose_write_nodes(non_neg_integer(), [node()], [node()]) -> {ok, [node()]}.
choose_write_nodes(K, Include, Exclude) ->
    gen_server:call(?MODULE, {choose_write_nodes, K, Include, Exclude}).

-spec get_tags(gc) -> {ok, [tagname()], [node()]} | too_many_failed_nodes;
              (safe) -> {ok, [binary()]} | too_many_failed_nodes.
get_tags(Mode) ->
    get_tags(?MODULE, Mode, ?GET_TAG_TIMEOUT).

-spec get_tags(server(), gc, non_neg_integer()) ->
                      {ok, [tagname()], [node()]} | too_many_failed_nodes;
              (server(), safe, non_neg_integer()) ->
                      {ok, [binary()]} | too_many_failed_nodes.
get_tags(Server, Mode, Timeout) ->
    disco_profile:timed_run(
        fun() -> gen_server:call(Server, {get_tags, Mode}, Timeout) end,
        get_tags).

-spec new_blob(string()|object_name(), non_neg_integer(), [node()], [node()]) ->
                      too_many_replicas | {ok, [nonempty_string()]}.
new_blob(Obj, K, Include, Exclude) ->
    gen_server:call(?MODULE, {new_blob, Obj, K, Include, Exclude}, infinity).

-spec new_blob(server(), string()|object_name(), non_neg_integer(), [node()], [node()]) ->
                      too_many_replicas | {ok, [nonempty_string()]}.
new_blob(Master, Obj, K, Include, Exclude) ->
    gen_server:call(Master, {new_blob, Obj, K, Include, Exclude}, infinity).

-spec safe_gc_blacklist() -> {ok, [node()]} | {error, term()}.
safe_gc_blacklist() ->
    gen_server:call(?MODULE, safe_gc_blacklist).

-spec safe_gc_blacklist(gb_sets:set()) -> ok.
safe_gc_blacklist(SafeGCBlacklist) ->
    gen_server:cast(?MODULE, {safe_gc_blacklist, SafeGCBlacklist}).

-spec update_gc_stats(gc_run_stats()) -> ok.
update_gc_stats(Stats) ->
    gen_server:cast(?MODULE, {update_gc_stats, Stats}).

-type nodes_update() :: [{node(), boolean(), boolean()}].
-spec update_nodes(nodes_update()) -> ok.
update_nodes(DDFSNodes) ->
    gen_server:cast(?MODULE, {update_nodes, DDFSNodes}).

-spec update_nodestats(gb_trees:tree()) -> ok.
update_nodestats(NewNodes) ->
    gen_server:cast(?MODULE, {update_nodestats, NewNodes}).

-spec update_tag_cache(gb_sets:set()) -> ok.
update_tag_cache(TagCache) ->
    gen_server:cast(?MODULE, {update_tag_cache, TagCache}).

-spec refresh_tag_cache() -> ok.
refresh_tag_cache() ->
    gen_server:cast(?MODULE, refresh_tag_cache).

%% ===================================================================
%% gen_server callbacks

-spec init(_) -> gs_init().
init(_Args) ->
    _ = [disco_profile:new_histogram(Name)
        || Name <- [get_tags, do_get_tags_all, do_get_tags_filter,
            do_get_tags_safe, do_get_tags_gc]],
    spawn_link(fun() -> monitor_diskspace() end),
    spawn_link(fun() -> ddfs_gc:start_gc(disco:get_setting("DDFS_DATA")) end),
    Refresher = spawn_link(fun() -> refresh_tag_cache_proc() end),
    put(put_port, disco:get_setting("DDFS_PUT_PORT")),
    {ok, #state{cache_refresher = Refresher}}.

-type choose_write_nodes_msg() :: {choose_write_nodes, non_neg_integer(), [node()], [node()]}.
-type new_blob_msg() :: {new_blob, string() | object_name(), non_neg_integer(), [node()]}.
-type tag_msg() :: {tag, ddfs_tag:call_msg(), tagname()}.
-spec handle_call(dbg_state_msg(), from(), state()) ->
                         gs_reply(state());
                 ({get_nodeinfo, all}, from(), state()) ->
                         gs_reply({ok, [node_info()]});
                 (get_read_nodes, from(), state()) ->
                         gs_reply({ok, [node()], non_neg_integer});
                 (gc_blacklist, from(), state()) ->
                         gs_reply({ok, [node()]});
                 (gc_stats, from(), state()) ->
                         gs_reply({ok, gc_stats(), erlang:timestamp()});
                 (choose_write_nodes_msg(), from(), state()) ->
                         gs_reply({ok, [node()]});
                 (new_blob_msg(), from(), state()) ->
                         gs_reply(new_blob_result());
                 (tag_msg(), from(), state()) ->
                         gs_reply({error, nonodes}) | gs_noreply();
                 ({get_tags, gc | safe}, from(), state()) ->
                         gs_noreply();
                 ({get_hosted_tags, host()}, from(), state()) ->
                         gs_noreply();
                 (safe_gc_blacklist, from(), state()) ->
                         gs_reply({ok, [node()]}).
handle_call(dbg_get_state, _, S) ->
    {reply, S, S};

handle_call({get_nodeinfo, all}, _From, #state{nodes = Nodes} = S) ->
    {reply, {ok, Nodes}, S};

handle_call(get_read_nodes, _F, #state{nodes = Nodes, read_blacklist = RB} = S) ->
    {reply, do_get_readable_nodes(Nodes, RB), S};

handle_call(gc_blacklist, _F, #state{gc_blacklist = Nodes} = S) ->
    {reply, {ok, Nodes}, S};

handle_call(gc_stats, _F, #state{gc_stats = Stats} = S) ->
    {reply, {ok, Stats}, S};

handle_call({choose_write_nodes, K, Include, Exclude}, _,
            #state{nodes = N, write_blacklist = WBL, gc_blacklist = GBL} = S) ->
    BL = lists:umerge(WBL, GBL),
    {reply, do_choose_write_nodes(N, K, Include, Exclude, BL), S};

handle_call({new_blob, Obj, K, Include, Exclude}, _,
            #state{nodes = N, gc_blacklist = GBL, write_blacklist = WBL} = S) ->
    BL = lists:umerge(WBL, GBL),
    {reply, do_new_blob(Obj, K, Include, Exclude, BL, N), S};

handle_call({tag, _M, _Tag}, _From, #state{nodes = []} = S) ->
    {reply, {error, no_nodes}, S};

handle_call({tag, M, Tag}, From, S) ->
    {noreply, do_tag_request(M, Tag, From, S)};

handle_call({get_tags, Mode}, From, #state{nodes = Nodes} = S) ->
    spawn(fun() ->
              gen_server:reply(From, do_get_tags(Mode, [N || {N, _} <- Nodes]))
          end),
    {noreply, S};

handle_call({get_hosted_tags, Host}, From, S) ->
    spawn(fun() -> gen_server:reply(From, ddfs_gc:hosted_tags(Host)) end),
    {noreply, S};

handle_call(safe_gc_blacklist, _From, #state{safe_gc_blacklist = SBL} = S) ->
    {reply, {ok, gb_sets:to_list(SBL)}, S}.

-spec handle_cast({tag_notify, ddfs_tag:cast_msg(), tagname()}
                  | {gc_blacklist, [node()]}
                  | {safe_gc_blacklist, gb_sets:set()}
                  | {update_gc_stats, gc_stats()}
                  | {update_tag_cache, gb_sets:set()}
                  | refresh_tag_cache
                  | {update_nodes, nodes_update()}
                  | {update_nodestats, gb_trees:tree()},
                  state()) -> gs_noreply().
handle_cast({tag_notify, M, Tag}, S) ->
    {noreply, do_tag_notify(M, Tag, S)};

handle_cast({gc_blacklist, Nodes}, #state{safe_gc_blacklist = SBL} = S) ->
    BLSet = gb_sets:from_list(Nodes),
    NewSBL  = gb_sets:intersection(BLSet, SBL),
    {noreply, S#state{gc_blacklist = gb_sets:to_list(BLSet),
                      safe_gc_blacklist = NewSBL}};

handle_cast({safe_gc_blacklist, SafeBlacklist}, #state{gc_blacklist = BL} = S) ->
    SBL = gb_sets:intersection(SafeBlacklist, gb_sets:from_list(BL)),
    {noreply, S#state{safe_gc_blacklist = SBL}};

handle_cast({update_gc_stats, Stats}, S) ->
    {noreply, S#state{gc_stats = {Stats, now()}}};

handle_cast({update_tag_cache, TagCache}, S) ->
    {noreply, S#state{tag_cache = TagCache}};

handle_cast(refresh_tag_cache, #state{cache_refresher = Refresher} = S) ->
    Refresher ! refresh,
    {noreply, S};

handle_cast({update_nodes, NewNodes}, S) ->
    {noreply, do_update_nodes(NewNodes, S)};

handle_cast({update_nodestats, NewNodes}, S) ->
    {noreply, do_update_nodestats(NewNodes, S)}.

-spec handle_info({'DOWN', _, _, pid(), _}, state()) -> gs_noreply().
handle_info({'DOWN', _, _, Pid, _}, S) ->
    {noreply, do_tag_exit(Pid, S)}.

%% ===================================================================
%% gen_server callback stubs

-spec terminate(term(), state()) -> ok.
terminate(Reason, _State) ->
    lager:warning("DDFS master died: ~p", [Reason]).

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% internal functions

-spec do_get_readable_nodes([node_info()], [node()]) ->
                                   {ok, [node()], non_neg_integer()}.
do_get_readable_nodes(Nodes, ReadBlacklist) ->
    NodeSet = gb_sets:from_ordset(lists:sort([Node || {Node, _} <- Nodes])),
    BlackSet = gb_sets:from_ordset(ReadBlacklist),
    ReadableNodeSet = gb_sets:subtract(NodeSet, BlackSet),
    {ok, gb_sets:to_list(ReadableNodeSet), gb_sets:size(BlackSet)}.

-spec do_choose_write_nodes([node_info()], non_neg_integer(), [node()], [node()], [node()]) ->
                                   {ok, [node()]}.
do_choose_write_nodes(Nodes, K, Include, Exclude, BlackList) ->
    % Include is the list of nodes that must be included
    %
    % Node selection algorithm:
    % 1. try to choose K nodes randomly from all the nodes which have
    %    more than ?MIN_FREE_SPACE bytes free space available and which
    %    are not excluded or blacklisted.
    % 2. if K nodes cannot be found this way, choose the K emptiest
    %    nodes which are not excluded or blacklisted.
    Primary = ([N || {N, {Free, _Total}} <- Nodes, Free > ?MIN_FREE_SPACE / 1024]
               -- (Exclude ++ BlackList)),
    if length(Primary) >= K ->
            {ok, Include ++ disco_util:choose_random(Primary -- Include , K - length(Include))};
       true ->
            Preferred = [N || {N, _} <- lists:reverse(lists:keysort(2, Nodes))],
            Secondary = Include ++ lists:sublist(Preferred -- (Include ++ Exclude ++ BlackList),
                                                 K - length(Include)),
            {ok, Secondary}
    end.

-type new_blob_result() :: too_many_replicas | {ok, [nonempty_string()]}.
-spec do_new_blob(string()|object_name(), non_neg_integer(), [node()], [node()], [node()], [node_info()]) ->
                         new_blob_result().
do_new_blob(_Obj, K, _Include, _Exclude, _BlackList, Nodes) when K > length(Nodes) ->
    too_many_replicas;
do_new_blob(Obj, K, Include, Exclude, BlackList, Nodes) ->
    {ok, WriteNodes} = do_choose_write_nodes(Nodes, K, Include, Exclude, BlackList),
    Urls = [["http://", disco:host(N), ":", get(put_port), "/ddfs/", Obj]
            || N <- WriteNodes],
    {ok, Urls}.

% Tag request: Start a new tag server if one doesn't exist already. Forward
% the request to the tag server.

-spec get_tag_pid(tagname(), gb_trees:tree(), false | gb_sets:set()) ->
                         {pid(), gb_trees:tree()}.
get_tag_pid(Tag, Tags, Cache) ->
    case gb_trees:lookup(Tag, Tags) of
        none ->
            NotFound = (Cache =/= false
                        andalso not gb_sets:is_element(Tag, Cache)),
            {ok, Server} = ddfs_tag:start(Tag, NotFound),
            erlang:monitor(process, Server),
            {Server, gb_trees:insert(Tag, Server, Tags)};
        {value, P} ->
            {P, Tags}
    end.

-spec do_tag_request(term(), tagname(), replyto(), state()) ->
                            state().
do_tag_request(M, Tag, From, #state{tags = Tags, tag_cache = Cache} = S) ->
    {Pid, TagsN} = get_tag_pid(Tag, Tags, Cache),
    gen_server:cast(Pid, {M, From}),
    S#state{tags = TagsN,
            tag_cache = Cache =/= false andalso gb_sets:add(Tag, Cache)}.

-spec do_tag_notify(term(), tagname(), state()) -> state().
do_tag_notify(M, Tag, #state{tags = Tags, tag_cache = Cache} = S) ->
    {Pid, TagsN} = get_tag_pid(Tag, Tags, Cache),
    gen_server:cast(Pid, {notify, M}),
    S#state{tags = TagsN,
            tag_cache = Cache =/= false andalso gb_sets:add(Tag, Cache)}.

-spec do_update_nodes(nodes_update(), state()) -> state().
do_update_nodes(NewNodes, #state{nodes = Nodes, tags = Tags} = S) ->
    WriteBlacklist = lists:sort([Node || {Node, false, _} <- NewNodes]),
    ReadBlacklist = lists:sort([Node || {Node, _, false} <- NewNodes]),
    OldNodes = gb_trees:from_orddict(Nodes),
    UpdatedNodes = lists:keysort(1, [case gb_trees:lookup(Node, OldNodes) of
                                         none ->
                                             {Node, {0, 0}};
                                         {value, OldStats} ->
                                             {Node, OldStats}
                                     end || {Node, _WB, _RB} <- NewNodes]),
    if
        UpdatedNodes =/= Nodes ->
            _ = [gen_server:cast(Pid, {die, none}) || Pid <- gb_trees:values(Tags)],
            spawn(fun() ->
                          {ok, ReadableNodes, RBSize} =
                              do_get_readable_nodes(UpdatedNodes, ReadBlacklist),
                          refresh_tag_cache(ReadableNodes, RBSize)
                  end),
            S#state{nodes = UpdatedNodes,
                    write_blacklist = WriteBlacklist,
                    read_blacklist = ReadBlacklist,
                    tag_cache = false,
                    tags = gb_trees:empty()};
        true ->
            S#state{write_blacklist = WriteBlacklist,
                    read_blacklist = ReadBlacklist}
    end.

-spec do_update_nodestats(gb_trees:tree(), state()) -> state().
do_update_nodestats(NewNodes, #state{nodes = Nodes} = S) ->
    UpdatedNodes = [case gb_trees:lookup(Node, NewNodes) of
                        none ->
                            {Node, Stats};
                        {value, NewStats} ->
                            {Node, NewStats}
                    end || {Node, Stats} <- Nodes],
    S#state{nodes = UpdatedNodes}.

-spec do_tag_exit(pid(), state()) -> state().
do_tag_exit(Pid, S) ->
    NewTags = [X || {_, V} = X <- gb_trees:to_list(S#state.tags), V =/= Pid],
    S#state{tags = gb_trees:from_orddict(NewTags)}.

-spec do_get_tags(all | filter, [node()]) -> {[node()], [node()], [binary()]};
                 (safe, [node()]) -> {ok, [binary()]} | too_many_failed_nodes;
                 (gc, [node()]) -> {ok, [binary()], [node()]} | too_many_failed_nodes.
do_get_tags(all, Nodes) ->
    disco_profile:timed_run(
        fun() ->
            {Replies, Failed} =
                gen_server:multi_call(Nodes, ddfs_node, get_tags, ?NODE_TIMEOUT),
            {OkNodes, Tags} = lists:unzip(Replies),
            {OkNodes, Failed, lists:usort(lists:flatten(Tags))}
        end, do_get_tags_all);

do_get_tags(filter, Nodes) ->
    disco_profile:timed_run(
        fun() ->
            {OkNodes, Failed, Tags} = do_get_tags(all, Nodes),
            case tag_operation(get_tagnames, <<"+deleted">>, ?NODEOP_TIMEOUT) of
                {ok, Deleted} ->
                    TagSet = gb_sets:from_ordset(Tags),
                    DelSet = gb_sets:insert(<<"+deleted">>, Deleted),
                    NotDeleted = gb_sets:to_list(gb_sets:subtract(TagSet, DelSet)),
                    {OkNodes, Failed, NotDeleted};
                E ->
                    E
            end
        end, do_get_tags_filter);

do_get_tags(safe, Nodes) ->
    disco_profile:timed_run(
        fun() ->
            TagMinK = list_to_integer(disco:get_setting("DDFS_TAG_MIN_REPLICAS")),
            case do_get_tags(filter, Nodes) of
                {_OkNodes, Failed, Tags} when length(Failed) < TagMinK ->
                    {ok, Tags};
                _ ->
                    too_many_failed_nodes
            end
        end, do_get_tags_safe);

% The returned tag list may include +deleted.
do_get_tags(gc, Nodes) ->
    disco_profile:timed_run(
        fun() ->
            {OkNodes, Failed, Tags} = do_get_tags(all, Nodes),
            TagMinK = list_to_integer(disco:get_setting("DDFS_TAG_MIN_REPLICAS")),
            case length(Failed) < TagMinK of
                false ->
                    too_many_failed_nodes;
                true ->
                    case tag_operation(get_tagnames, <<"+deleted">>, ?NODEOP_TIMEOUT) of
                        {ok, Deleted} ->
                            TagSet = gb_sets:from_ordset(Tags),
                            NotDeleted = gb_sets:subtract(TagSet, Deleted),
                            {ok, gb_sets:to_list(NotDeleted), OkNodes};
                        E ->
                            E
                    end
            end
        end, do_get_tags_gc).

% Timeouts in this call by the below processes can cause ddfs_master
% itself to crash, since the processes are linked to it.
-spec safe_get_read_nodes() -> {ok, [node()], non_neg_integer()} | error.
safe_get_read_nodes() ->
    try get_read_nodes() of
        {ok, _ReadableNodes, _RBSize} = RN ->
            RN;
        E ->
            lager:error("unexpected response retrieving readable nodes: ~p", [E]),
            error
    catch
        K:E ->
            lager:error("error retrieving readable nodes: ~p:~p", [K, E]),
            error
    end.

-spec monitor_diskspace() -> no_return().
monitor_diskspace() ->
    case safe_get_read_nodes() of
        {ok, ReadableNodes, _RBSize} ->
            {Space, _F} = gen_server:multi_call(ReadableNodes,
                                                ddfs_node,
                                                get_diskspace,
                                                ?NODE_TIMEOUT),
            update_nodestats(gb_trees:from_orddict(lists:keysort(1, Space)));
        error ->
            ok
    end,
    timer:sleep(?DISKSPACE_INTERVAL),
    monitor_diskspace().

-spec refresh_tag_cache_proc() -> no_return().
refresh_tag_cache_proc() ->
    case safe_get_read_nodes() of
        {ok, ReadableNodes, RBSize} ->
            refresh_tag_cache(ReadableNodes, RBSize);
        error ->
            ok
    end,
    receive
        refresh ->
            ok
    after ?TAG_CACHE_INTERVAL ->
            ok
    end,
    refresh_tag_cache_proc().

-spec refresh_tag_cache([node()], non_neg_integer()) -> ok.
refresh_tag_cache(Nodes, BLSize) ->
    TagMinK = list_to_integer(disco:get_setting("DDFS_TAG_MIN_REPLICAS")),
    {Replies, Failed} =
        gen_server:multi_call(Nodes, ddfs_node, get_tags, ?NODE_TIMEOUT),
    if Nodes =/= [], length(Failed) + BLSize < TagMinK ->
            {_OkNodes, Tags} = lists:unzip(Replies),
            update_tag_cache(gb_sets:from_list(lists:flatten(Tags)));
       true -> ok
    end.
