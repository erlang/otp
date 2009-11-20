-module(img).

-export([do/0]).

do() ->
    Im = egd:create(200,200),
    Red = egd:color({255,0,0}),
    Green = egd:color({0,255,0}),
    Blue = egd:color({0,0,255}),
    Black = egd:color({0,0,0}),
    Yellow = egd:color({255,255,0}),

    % Line and fillRectangle

    egd:filledRectangle(Im, {20,20}, {180,180}, Red),
    egd:line(Im, {0,0}, {200,200}, Black),    

    egd:save(egd:render(Im, png), "/home/egil/test1.png"),
    
    egd:filledEllipse(Im, {45, 60}, {55, 70}, Yellow),
    egd:filledEllipse(Im, {145, 60}, {155, 70}, Blue),

    egd:save(egd:render(Im, png), "/home/egil/test2.png"),

    R = 80,
    X0 = 99,
    Y0 = 99,

    Pts = [ { 	X0 + trunc(R*math:cos(A*math:pi()*2/360)),
		Y0 + trunc(R*math:sin(A*math:pi()*2/360))
	    } || A <- lists:seq(0,359,5)],
    lists:map(
	fun({X,Y}) ->
	    egd:rectangle(Im, {X-5, Y-5}, {X+5,Y+5}, Green)
	end, Pts), 

    egd:save(egd:render(Im, png), "/home/egil/test3.png"),

    % Text
    Filename = filename:join([code:priv_dir(percept), "fonts", "6x11_latin1.wingsfont"]),
    Font = egd_font:load(Filename),
    {W,H} = egd_font:size(Font),
    String = "egd says hello",
    Length = length(String),

    egd:text(Im, {round(100 - W*Length/2), 200 - H - 5}, Font, String, Black),
 
    egd:save(egd:render(Im, png), "/home/egil/test4.png"),

    egd:destroy(Im).
