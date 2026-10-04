#!/usr/bin/env escript

-define(SNAKE_WIDTH, 60).  
-define(SNAKE_HEIGHT, 20). 
-define(EMPTY, $ ).
-define(SNAKE, $@).
-define(FOOD, $*).
-define(BORDER, $#).
-define(FOOD_CHAR, $*).
-define(INITIAL_SPEED, 150).
-define(SPEED_INCREMENT, 5).   
-define(MIN_SPEED, 30).        


-define(BLUECOLOR, "\x1b[34m").  % Added game colors
-define(GREENCOLOR, "\x1b[32m").
-define(REDCOLOR, "\x1b[31m").    % Added red color
-define(RESET,"\x1b[0m").

main(_) ->
      io:format("~s~n
           Welcome to snake game                                                                                                                                                           
      designed by Ericsson~n~s", [?BLUECOLOR, ?RESET]),
     io:format("~s~n"
         "Controls: ~n"
         "  Movement:   WASD or Arrow Keys ~n"
         "  Pause: P~n"
         "  Quit: Q ~n~n"
         "Eat the food (*) to grow! ~n"
         "Don't hit walls or yourself! ~n~n"
         "Good luck!~n~n~s",
         [?GREENCOLOR, ?RESET]),
    timer:sleep(4000),

    io:format("\e[?25l\e[?12l\e[?1c"),  % Hide cursor and disable echo
    ok = shell:start_interactive({noshell, raw}),

    % Set random seed
    rand:seed(exsplus, os:timestamp()),

    register(snake_game, self()),
    Snake = [{?SNAKE_WIDTH div 2, ?SNAKE_HEIGHT div 2}],
    Food = spawn_food(Snake),
    Score = 0,

    spawn(fun() -> input_loop() end),
    Speed = ?INITIAL_SPEED,
    
    % Initialize the game by drawing static elements once
    io:format("\e[2J\e[H"),
    io:format("~sScore: ~p  Speed: ~pms~n~s", [?GREENCOLOR, Score, Speed, ?RESET]),
    draw_borders(),

    draw_at(element(1, Food), element(2, Food), ?FOOD),
    [draw_at(X, Y, ?SNAKE) || {X,Y} <- Snake],
    
    loop(Snake, Food, 0, {1,0}, ?INITIAL_SPEED, Snake, Food).


loop(Snake, Food, Score, Direction, Speed, OldSnake, OldFood) ->
    % Only clear the parts that need to change
    clear_old_positions(OldSnake, OldFood, Snake, Food),
    
    % Draw new positions
    draw_at(element(1, Food), element(2, Food), ?FOOD),
    [draw_at(X, Y, ?SNAKE) || {X,Y} <- Snake],
    
    % Process any pending direction changes immediately
    NewDirection = process_inputs(Direction, Speed),
    
    % Calculate new head position
    {HX, HY} = hd(Snake),
    NewHead = case NewDirection of
        {0, -1} -> {HX, HY - 1};  % Up
        {0, 1} -> {HX, HY + 1};    % Down
        {-1, 0} -> {HX - 1, HY};   % Left
        {1, 0} -> {HX + 1, HY}     % Right
    end,
    
    case check_collision([NewHead|Snake]) of
        true -> game_over(Score, Speed);
        false ->
            % Update game state
            AteFood = NewHead =:= Food,
            NewSnake = if
                AteFood -> [NewHead | Snake];           % Grow snake
                true -> [NewHead | lists:droplast(Snake)] % Move snake
            end,
            NewFood = if AteFood -> spawn_food(NewSnake); true -> Food end,
            NewScore = if AteFood -> Score + 1; true -> Score end,
            
            % Update score display if needed
            case AteFood of
                true -> 
                    io:format("\e[1;1H"), % Move cursor to score position
                    io:format("~sScore: ~p  Speed: ~pms~s", [?GREENCOLOR, NewScore, max(?MIN_SPEED, Speed - ?SPEED_INCREMENT), ?RESET]);
                false -> ok
            end,
            
            % Increase speed when eating food 
            NewSpeed = if
                AteFood -> max(?MIN_SPEED, Speed - ?SPEED_INCREMENT);
                true -> Speed
            end,

            % Control game speed
            timer:sleep(NewSpeed),
            
            
            loop(NewSnake, NewFood, NewScore, NewDirection, NewSpeed, Snake, Food)
    end.

% Clear only the positions that need to be cleared
clear_old_positions(OldSnake, OldFood, NewSnake, NewFood) ->
    % Clear old snake tail (if it moved)
    case OldSnake =/= NewSnake of
        true -> 
            LastOld = lists:last(OldSnake),
            LastNew = lists:last(NewSnake),
            case LastOld =/= LastNew of
                true -> draw_at(element(1, LastOld), element(2, LastOld), ?EMPTY);
                false -> ok
            end;
        false -> ok
    end,
    
    % Clear old food if it was eaten
    case OldFood =/= NewFood of
        true -> draw_at(element(1, OldFood), element(2, OldFood), ?EMPTY);
        false -> ok
    end.

% draw elements based on char and colour
draw_at(X, Y, Char) ->
     Color = case Char of
        ?BORDER -> ?BLUECOLOR;
        ?SNAKE -> ?GREENCOLOR;
        ?FOOD -> ?REDCOLOR;
        _ -> ?RESET
    end,
     io:format("\e[~B;~BH~s~c~s", [Y + 3, X + 2, Color, Char, ?RESET]).

draw_borders() ->
    % Draw top and bottom borders
    [draw_at(X, 0, ?BORDER) || X <- lists:seq(0, ?SNAKE_WIDTH + 1)],
    [draw_at(X, ?SNAKE_HEIGHT + 1, ?BORDER) || X <- lists:seq(0, ?SNAKE_WIDTH + 1)],
    
    % Draw left and right borders
    [draw_at(0, Y, ?BORDER) || Y <- lists:seq(0, ?SNAKE_HEIGHT + 1)],
    [draw_at(?SNAKE_WIDTH + 1, Y, ?BORDER) || Y <- lists:seq(0, ?SNAKE_HEIGHT + 1)].

process_inputs(CurrentDir, Speed) ->
    receive
        {new_direction, NewDir} -> 
            % Prevent 180° turns
            case {CurrentDir, NewDir} of
                {{0,1},{0,-1}} -> CurrentDir;
                {{0,-1},{0,1}} -> CurrentDir;
                {{1,0},{-1,0}} -> CurrentDir;
                {{-1,0},{1,0}} -> CurrentDir;
                _ -> NewDir
            end;
        pause ->
            show_pause(),
            wait_for_unpause(),
            process_inputs(CurrentDir, Speed);
        quit ->
            exit(normal)
    after 0 -> CurrentDir
    end.

show_pause() ->
    io:format("\e[~B;~BH", [?SNAKE_HEIGHT + 6, 1]),
    io:format("~s[PAUSED] Press any key to continue...~s", [?REDCOLOR, ?RESET]).

wait_for_unpause() ->
    io:get_chars("", 1),
    % Clear pause message
    io:format("\e[~B;~BH\e[K", [?SNAKE_HEIGHT + 6, 1]).

input_loop() ->
    case io:get_chars("", 1) of
        {error, _} -> input_loop();
        "\e" -> 
            case io:get_chars("", 2) of
                "[A" -> snake_game ! {new_direction, {0, -1}};
                "[B" -> snake_game ! {new_direction, {0, 1}};
                "[C" -> snake_game ! {new_direction, {1, 0}};
                "[D" -> snake_game ! {new_direction, {-1, 0}};
                _ -> ok
            end;
        "w" -> snake_game ! {new_direction, {0, -1}};
        "W" -> snake_game ! {new_direction, {0, -1}};
        "s" -> snake_game ! {new_direction, {0, 1}};
        "S" -> snake_game ! {new_direction, {0, 1}};
        "a" -> snake_game ! {new_direction, {-1, 0}};
        "A" -> snake_game ! {new_direction, {-1, 0}};
        "d" -> snake_game ! {new_direction, {1, 0}};
        "D" -> snake_game ! {new_direction, {1, 0}};
        "p" -> snake_game ! pause;
        "P" -> snake_game ! pause;
        "q" -> snake_game ! quit;
        "Q" -> snake_game ! quit;
        _ -> ok
    end,
    input_loop().

game_over(Score, Speed) ->
    io:format(
        "\e[2J\e[H"    % Clear screen
        "~sGame Over!~n"
        "Final Score: ~p~n"
        "Final Speed: ~pms~n"
        "Press any key to exit~n",
        [?REDCOLOR, Score, Speed] ),
    io:get_chars("", 1),
    io:format("~s\e[?25h", [?RESET]),  % Reset color + show cursor
    halt().

check_collision([Head|Tail]) ->
    {X,Y} = Head,
    % Check if snake hit the walls 
    X =< 0 orelse X >= ?SNAKE_WIDTH + 1 orelse
    Y =< 0 orelse Y >= ?SNAKE_HEIGHT + 1 orelse
    lists:member(Head, Tail).

spawn_food(Snake) ->
    % Food should spawn inside the play area 
    X = rand:uniform(?SNAKE_WIDTH - 1) + 1,
    Y = rand:uniform(?SNAKE_HEIGHT - 1) + 1,
    case lists:member({X,Y}, Snake) of
        true -> spawn_food(Snake);
        false -> {X,Y}
    end.