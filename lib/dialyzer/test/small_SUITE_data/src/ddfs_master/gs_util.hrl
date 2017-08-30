% This is a set of type utilities to be used when spec-cing the
% callbacks of a gen_server implementation.  It should be included in
% the impl module, which needs to define the state() type.

-type gs_init()      :: {ok, state()}.
-type gs_reply(T)    :: {reply, (T), state()}.
-type gs_noreply()   :: {noreply, state()}.
-type gs_noreply_t() :: {noreply, state(), non_neg_integer()}.
-type gs_stop(T)     :: {stop, (T), state()}.

% Generic utilities.

-type server() :: pid() | atom() | {atom(), node()}.
-type from()   :: {pid(), term()}.

-type dbg_state_msg() :: dbg_get_state.
