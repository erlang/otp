%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

-module(mandel).
-author('(mbj,eklas)@erlang.ericsson.se').

%% User's interface
-export([start/0,start/1]).

%% Internal exports:
-export([start_client/2,refresher/1,start_server/1,respond/2]).

%%%-----------------------------------------------------------------
%%% Distributed Mandelbrot program.
%%% Originally written i C++/rpc/lwp/interviews by Klas Eriksson.(1200 lines)
%%% Rewritten in Erlang by Klas Eriksson and Martin Björklund.
%%%-----------------------------------------------------------------

%% unix>erl -sname foo            (all nodes will get the same name)
%% (foo@data)1>mandel:start([{hosts,["computer1","computer2"]},{window,400}]).

%% unix>erl
%% 1> mandel:start().

-record(state,{image,width,height,xmax,ymax,range,
	       maxiter,colortable,zoomstep}).
-record(job,{left,right,ymin,ymax,height,width,maxiter,data=[]}).
-define(JOBWIDTH,10).

%%-----------------------------------------------------------------
%% This is the client start function.
%%-----------------------------------------------------------------
start() ->
    start([]).

%%----------------------------------------------------------------------
%% Option is list of Option. Option is:
%% {xmax,float()}|{ymax,float()}|{range,float()}|
%% {maxiter,integer()}|{window,integer()}|{zoomstep,float()}|
%% {hosts,(list of string())|all_found_nodes}
%%----------------------------------------------------------------------
start(Opts) ->
    Nodes1 = nodes(),
    Nodes = case get_option(hosts,Opts,all_found_nodes) of
		all_found_nodes when Nodes1 == [] ->
		    N = [node()],
		    spawn(mandel,start_server,[N]),
		    N;
		all_found_nodes ->
		    start_nodes(dir(),Nodes1),
		    Nodes1;
		Hosts ->
		    start_slaves(Hosts),
		    start_nodes(dir(),Nodes1),
		    Nodes1
	    end,
    spawn(mandel,start_client,[Opts,Nodes]).
    
%% This is not an application so we don't have their way of knowing
%% a private data directory where the GIF files are located (this directory).
%% We can find GS and makes it relative from there /kgb

-define(EbinFromGsPriv,"../contribs/ebin").

dir()->
    GsPrivDir = code:priv_dir(gs),
    filename:join(GsPrivDir,?EbinFromGsPriv).


start_slaves([]) -> ok;
start_slaves([Host|Hs]) ->
    {ok,Name}=slave:start(Host),
    io:format("host ~p is up~n", [Name]),
    start_slaves(Hs).

start_nodes(_Dir,[]) -> ok;
start_nodes(Dir,[Node|Nodes]) ->
    rpc:call(Node,code,add_path,[Dir]), % hack? should be done in .erlang
    spawn_link(Node,mandel,start_server,[[node()]]),
    io:format("started mandelserver at node: ~p~n", [Node]),
    start_nodes(Dir,Nodes).

start_client(Opts,Nodes) ->
    Wt = get_option(window,Opts,100) div ?JOBWIDTH * ?JOBWIDTH,
    Ht = get_option(window,Opts,100) div ?JOBWIDTH * ?JOBWIDTH,
    S=gs:start(),
    Win=gs:create(window,win1,S,[{title,"Mandel"},{width,Wt-1},{height,Ht-1},
				{configure,true}]),
    Canvas=gs:create(canvas,can1,Win,[{width,Wt},{height,Ht}]),
    Image=gs:image(Canvas,[{buttonpress,true}]),
    MaxIters = get_option(maxiter,Opts,100),
    timer:apply_after(8000,mandel,refresher,[Image]),
    CT = make_color_table(MaxIters),
    State2=#state{image=Image,width=Wt,height=Ht,
		  xmax=try_random(get_option(xmax,Opts,2),-2,2),
		  ymax=try_random(get_option(ymax,Opts,2),-2,2),
		  range=try_random(get_option(range,Opts,4),0,4),
		  maxiter=MaxIters,colortable=CT,
		  zoomstep=get_option(zoomstep,Opts,1.7)},
    ToDo = make_jobs(State2),
    gs:config(Win,[{map,true}]),
    main(State2, [], Nodes, ToDo).

try_random(random,Low,High) ->
    random:uniform()*(High-Low)+Low;
try_random(Float,_Low,_High) when number(Float) -> Float.
    

%%-----------------------------------------------------------------
%% Distribute work to the nodes. When a node returns, that
%% node is the first to be used if there's any job left.
%%-----------------------------------------------------------------
main(State, [], PassiveNodes, []) ->
    wait_event(State,[],PassiveNodes,[]);
main(State, ActiveNodes, PassiveNodes, []) ->
    % No jobs left, but some nodes are still active.
    % Wait_Event for their results
    wait_event(State,ActiveNodes,PassiveNodes,[]);
main(State, ActiveNodes, [Node|PassiveNodes], [Job|ToDo]) ->
    % We have work to do, and at least one passive node.
    % Let him do it.
    distribute_job(Node, Job),
    main(State, [Node|ActiveNodes], PassiveNodes, ToDo);
main(State, ActiveNodes, [], ToDo) ->
    % We have work to do, but all nodes are active.
    _Node = wait_event(State,ActiveNodes,[],ToDo).
    
wait_event(State,ActiveNodes,PassiveNodes,ToDo) ->
    receive
	{calculation_done, {Node, Job}} ->
	    if   % a small hack. we want to discard data for old pictures
		Job#job.ymax==State#state.ymax ->
		    draw(State, Node, Job);
		true -> true
	    end,
	    main(State,lists:delete(Node,ActiveNodes),[Node|PassiveNodes],ToDo);
	{gs,_Img,buttonpress,_Data,[_Butt,X,Y|_]} ->
	    #state{width=W,height=H,ymax=Ymax,xmax=Xmax,range=R,zoomstep=ZS} =
		State,
	    RX = Xmax-R+(X/W)*R,
	    RY = Ymax-R+(1-(Y/H))*R,
	    R2 = R/ZS,
	    Xmax2 = RX + R2/2,
	    Ymax2 = RY + R2/2,
	    State2 = State#state{xmax=Xmax2,ymax=Ymax2,range=R2},
	    io:format("{xmax,~w},{ymax,~w},{range,~w}~n", [Xmax2,Ymax2,R2]),
	    ToDo2=make_jobs(State2),
	    main(State2,ActiveNodes,PassiveNodes,ToDo2);
	{gs,_Win,destroy,_,_} ->
	    kill_nodes(lists:append(ActiveNodes,PassiveNodes));
	{gs,_Win,configure,_Data,[W,H|_]}
	when State#state.width==W+1, State#state.height==H+1->
	    main(State,ActiveNodes,PassiveNodes,ToDo);
	{gs,_Win,configure,_Data,[W|_]} ->
	    gs:config(can1,[{width,W},{height,W}]),
	    gs:config(win1,{configure,false}),
	    gs:config(win1,[{width,W-1},{height,W-1}]),
	    gs:config(win1,{configure,true}),
	    State2 = State#state{width=W,height=W},
	    ToDo2=make_jobs(State2),
	    main(State2,ActiveNodes,PassiveNodes,ToDo2)
    end.

kill_nodes([]) ->
    done;
kill_nodes([Node|Nodes]) ->
    exit(rpc:call(Node,erlang,whereis,[mandel_server]),kill),
    kill_nodes(Nodes).
    

distribute_job(Node, Job) ->
    {mandel_server, Node} ! {mandel_job, {self(), Job}}.

draw(#state{image=Image, width=Wt, height=Ht, xmax=Xmax,
		maxiter=MI,colortable=ColorTable,range=R}, Node, Job) ->
    #job{left=Left,data=Data}=Job,
    io:format("Got data from node ~30w~n", [Node]),
%% PixelX = K * RealX + M
%% 0      = K * Xmin  + M
%% Width-1= K * Xmax  + M
    K=(1-Wt)/-R,
    M=Wt-1-K*Xmax,
    Xbegin = round(Left*K+M),
    draw_cols(Image, Xbegin, Ht, lists:reverse(Data),MI,ColorTable).

draw_cols(Image, X, Ht, [H|T],MaxIter,ColorTable) ->
    draw_col(Image, X, 0, H,MaxIter,ColorTable),
    draw_cols(Image, X+1, Ht, T,MaxIter,ColorTable);
draw_cols(_Image, _X, _, [],_MaxIter,_ColorTable) ->
    done.

draw_col(_Image, _X,_Y,[{no_first_color,0}],_MaxIter,_ColorTable) ->
    done;
draw_col(Image, X,Y,[{Color,1}|T],MaxIter,ColorTable) ->
    gs:config(Image,[{pix_val,{{X,Y},
			       element(Color+1,ColorTable)}}]),
    draw_col(Image, X,Y+1,T,MaxIter,ColorTable);
draw_col(Image, X,Y,[{Color,Height}|T],MaxIter,ColorTable) ->
    gs:config(Image,[{pix_val,{{{X,Y},{X+1,Y+Height}},
			       element(Color+1,ColorTable)}}]),
    draw_col(Image, X,Y+Height,T,MaxIter,ColorTable).

make_jobs(#state{width=W,height=H,range=R,
		 xmax=Xmax,ymax=Ymax,maxiter=MI}) ->
    make_jobs(Xmax-R,Xmax,Ymax-R,Ymax,H,W,MI).

make_jobs(Xmin,Xmax,Ymin,Ymax,Ht,Wt,MaxIter) ->
    NoJobs = Wt/?JOBWIDTH,  % Each job is ?JOBWIDTH pixel-col
    DX = (Xmax - Xmin)/NoJobs,
    make_jobs(DX,Xmin,Xmax,#job{ymin=Ymin,ymax=Ymax,height=Ht,width=Wt/NoJobs,
				  maxiter=MaxIter},[]).

make_jobs(DX,Left,Xmax,JobSkel,Res) when Left =< Xmax ->
    Right = Left + DX,
    Job = JobSkel#job{left=Left,right=Right},
    make_jobs(DX,Right,Xmax,JobSkel,[Job | Res]);
make_jobs(_DX,_Left,_Xmax,_JobSkel,Res)  -> Res.
    
%%----------------------------------------------------------------------
%% A small process that refreshes the screen now and then.
%%----------------------------------------------------------------------
refresher(Image) ->
    gs:config(Image,flush),
    timer:apply_after(8000,mandel,refresher,[Image]).

%%-----------------------------------------------------------------
%% This is the server start function.
%%-----------------------------------------------------------------
start_server([ClientNode]) ->
    register(mandel_server, self()),
    erlang:monitor_node(ClientNode, true),
    server_loop().

server_loop() ->
    receive
	{mandel_job, {Pid, Job}} ->
	    spawn_link(mandel, respond, [Pid, Job]),
	    server_loop()
    end.
    
respond(Pid, Job) ->
    Data = do_job(Job),
    Pid ! {calculation_done, {node(), Data}}.

do_job(Job) ->
    calculate_area(Job).

calculate_area(Job) ->
    #job{ymin=Ymin,ymax=Ymax,height=Ht,width=Wt,left=Xmin,right=Xmax}=Job,
    Job#job{data=x_loop(0,[],Wt,(Xmax-Xmin)/Wt,(Ymax-Ymin)/Ht,Xmin,Job)}.

x_loop(IX,Res,Wt,Dx,Dy,X,Job) when IX < Wt ->
    #job{ymin=Ymin,height=Ht,maxiter=MaxIter}=Job,
    Cols = y_loop(0,Ht,[],MaxIter,Dy,X,Ymin,no_first_color,0),
    x_loop(IX+1,[Cols|Res],Wt,Dx,Dy,X+Dx,Job);
x_loop(_,Res,_,_,_,_,_) ->
    Res.

y_loop(IY,Ht,Res,MaxIter,Dy,X,Y,PrevColor,NprevColor) when IY < Ht -> 
    Color = color_loop(1,MaxIter,0,0,0,0,X,Y),
    if
	Color == PrevColor ->
	    y_loop(IY+1,Ht,Res,MaxIter,Dy,X,Y+Dy,PrevColor,NprevColor+1);
	true ->
	    y_loop(IY+1,Ht,[{PrevColor,NprevColor}|Res],MaxIter,
		   Dy,X,Y+Dy,Color,1)
    end;

y_loop(_,_,Res,_,_,_,_,PC,N) -> 
    [{PC,N}|Res].

color_loop(Color,MaxIter,Za,Zb,Za2,Zb2,X,Y) 
            when Za2 + Zb2 < 4, Color < MaxIter->
    Ztmp = Za2 - Zb2 + X,
    ZbN = 2 * Za * Zb + Y,
    color_loop(Color+1,MaxIter,Ztmp,ZbN,Ztmp * Ztmp,ZbN * ZbN,X,Y);
color_loop(MaxIter,MaxIter,_Za,_Zb,_Za2,_Zb2,_X,_Y) ->
    0; % black
color_loop(Color,_,_,_,_,_,_,_) ->
    Color.

%%----------------------------------------------------------------------
%% The "colormodel".
%%----------------------------------------------------------------------
make_color_table(MaxColors) ->
    list_to_tuple([{0,0,0}|colors(MaxColors)]).

colors(Ncolors) ->
    {A,B,C}=erlang:now(),
    random:seed(A,B,C),
    Colors = random_colors(Ncolors),
    Colors2 = best_insert([hd(Colors)],tl(Colors)),
    Colors2.

random_colors(0) -> [];
random_colors(N) ->
    R = random:uniform(256)-1,
    G = random:uniform(256)-1,
    B = random:uniform(256)-1,
   [{R,G,B}|random_colors(N-1)].

best_insert(Sorted,[RGB|Unsorted]) ->
    best_insert(insert_at(best_pos(RGB,Sorted),RGB,Sorted),Unsorted);
best_insert(Sorted,[]) -> Sorted.

insert_at(1,Elem,L) -> [Elem|L];
insert_at(N,Elem,[H|T]) -> [H|insert_at(N-1,Elem,T)].

best_pos(RGB, Sorted) ->
    D = distances(RGB,Sorted),
    pos_for_smallest_distance(D,1,1000,-1).

pos_for_smallest_distance([],_CurPos,_SmallestDist,Pos) -> Pos;
pos_for_smallest_distance([Dist|T],CurPos,SmallDist,_Pos)
 when Dist < SmallDist ->
    pos_for_smallest_distance(T,CurPos+1,Dist,CurPos);
pos_for_smallest_distance([_|T],CurPos,Smallest,Pos) ->
    pos_for_smallest_distance(T,CurPos+1,Smallest,Pos).

distances(_RGB,[]) ->
    [];
distances({R,G,B},[{R2,G2,B2}|T]) ->
    [lists:max([abs(R-R2),abs(G-G2),abs(B-B2)])|distances({R,G,B},T)].

get_option(Option, Options, Default) ->
    case gs:assq(Option, Options) of
	{value, Val} -> Val;
	false -> Default
    end.
