-module(my_behaviour).

-callback callback_init(Parent :: pid()) -> {'ok', State::term()}.

-callback callback_cast(State::term(), From::pid(), Msg::term()) ->
    {'noreply', NewState::term()}.

-callback callback_call(State::term(), From::pid(), Msg::term()) ->
    {'reply', NewState::term(), Reply::term()}.

-callback callback_exit(State::term()) -> 'ok'.
