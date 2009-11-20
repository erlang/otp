-module(img_esi).

-export([image/3]).

image(SessionID, _Env, _Input) ->
    mod_esi:deliver(SessionID, header()),
    Binary = my_image(),
    mod_esi:deliver(SessionID, binary_to_list(Binary)).

my_image() ->
    Im = egd:create(300,20),
    Black = egd:color({0,0,0}),
    Red = egd:color({255,0,0}),
    egd:filledRectangle(Im, {30,14}, {270,19}, Red),
    egd:rectangle(Im, {30,14}, {270,19}, Black),

    Filename = filename:join([code:priv_dir(percept), "fonts", "6x11_latin1.wingsfont"]),
    Font = egd_font:load(Filename),
    egd:text(Im, {30, 0}, Font, "egd with esi callback", Black),
    Bin = egd:render(Im, png),
    egd:destroy(Im),
    Bin.

header() ->
    "Content-Type: image/png\r\n\r\n".
