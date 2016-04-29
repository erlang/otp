%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%%-------------------------------------------------------------------
%%% File    : wx_opengl_SUITE.erl
%%% Author  : Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% Description : Test opengl basics, i.e. functions can be loaded, glu works.
%%% Created :  3 Nov 2008 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------
-module(wx_opengl_SUITE).
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include("wx_test_lib.hrl").
-include_lib("wx/include/gl.hrl").

%% Initialization functions.
init_per_suite(Config) ->
    case wx_test_lib:init_per_suite(Config) of
	Skipped = {skipped, _} -> Skipped;
	Config2 ->
	    case wx_test_lib:user_available(Config2) of
		true ->  Config2;
		false -> {skipped, "Ignoring opengl tests: run manually"}
	    end
    end.

end_per_suite(Config) ->
    wx_test_lib:end_per_suite(Config).

init_per_testcase(Func,Config) ->
    wx_test_lib:init_per_testcase(Func,Config).
end_per_testcase(Func,Config) -> 
    wx_test_lib:end_per_testcase(Func,Config).

%% SUITE specification
suite() -> [{ct_hooks,[ts_install_cth]}, {timetrap,{minutes,2}}].

all() -> 
    [canvas, glu_tesselation].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% The test cases

-define(VS, {{ 0.5,  0.5, -0.5},  %1
	     { 0.5, -0.5, -0.5},  %2
	     {-0.5, -0.5, -0.5},   
	     {-0.5,  0.5, -0.5},  %4
	     {-0.5,  0.5,  0.5},
	     { 0.5,  0.5,  0.5},  %6
	     { 0.5, -0.5,  0.5}, 
	     {-0.5, -0.5,  0.5}}).%8

-define(FACES, 
	%% Faces    Normal   
	[{{1,2,3,4},{0,0,-1} },   % 
	 {{3,8,5,4},{-1,0,0}},   %
	 {{1,6,7,2},{1,0,0} },   %
	 {{6,5,8,7},{0,0,1} },   %
	 {{6,1,4,5},{0,1,0} },   %
	 {{7,8,3,2},{0,-1,0}}]).


%% Test we can create a glCanvas and that functions are loaded dynamicly
canvas(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
canvas(Config) ->
    WX = ?mr(wx_ref, wx:new()),
    Frame = wxFrame:new(WX,1,"Hello 3D-World",[]),
    Attrs = [{attribList, [?WX_GL_RGBA,
			   ?WX_GL_DOUBLEBUFFER,
			   ?WX_GL_MIN_RED,8,
			   ?WX_GL_MIN_GREEN,8,
			   ?WX_GL_MIN_BLUE,8,
			   ?WX_GL_DEPTH_SIZE,24,0]}],
    Canvas = ?mt(wxGLCanvas, wxGLCanvas:new(Frame, [{style,?wxFULL_REPAINT_ON_RESIZE}|
						    Attrs])),
    
    wxFrame:connect(Frame, show),
    ?m(true, wxWindow:show(Frame)),

    receive #wx{event=#wxShow{}} -> ok
    after 1000 -> exit(show_timeout)
    end,

    ?m(false, wx:is_null(wxGLCanvas:getContext(Canvas))),
    ?m({'EXIT', {{error, no_gl_context,_},_}}, gl:getString(?GL_VENDOR)),

    ?m(ok, wxGLCanvas:setCurrent(Canvas)),
    io:format("Vendor:     ~s~n",  [gl:getString(?GL_VENDOR)]),
    io:format("Renderer:   ~s~n",  [gl:getString(?GL_RENDERER)]),
    io:format("Version:    ~s~n",  [gl:getString(?GL_VERSION)]),
    
    {W,H} = ?m({_,_}, wxWindow:getClientSize(Canvas)),
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    %%gl:frustum( -2.0, 2.0, -2.0, 2.0, 5.0, 25.0 ),
    gl:ortho( -2.0, 2.0, -2.0*H/W, 2.0*H/W, -20.0, 20.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LESS),
    {R,G,B,_} = wxWindow:getBackgroundColour(Frame),
    gl:clearColor(R/255,B/255,G/255,1.0),
    Data = {?FACES,?VS},
    drawBox(0, Data),
    ?m(ok, wxGLCanvas:swapBuffers(Canvas)),
    ?m([], flush()),
    Env = wx:get_env(),
    Tester = self(),
    spawn_link(fun() ->
		       wx:set_env(Env),
		       ?m(ok, wxGLCanvas:setCurrent(Canvas)),
		       ?m(ok, drawBox(1, Data)),
		       ?m(ok, wxGLCanvas:swapBuffers(Canvas)),
		       Tester ! works,
		       %% This may fail when window is deleted
		       catch draw_loop(2,Data,Canvas)
	       end),
    %% Needed on mac with wx-2.9
    wxGLCanvas:connect(Canvas, paint, 
    		       [{callback, fun(_,_) ->
    					   wxGLCanvas:setCurrent(Canvas),
    					   DC= wxPaintDC:new(Canvas),
    					   wxPaintDC:destroy(DC)
    				   end}]),


    ?m_receive(works),
    ?m([], flush()),
    io:format("Undef func ~p ~n", [catch gl:uniform1d(2, 0.75)]),
    timer:sleep(500),
    flush(),
    wx_test_lib:wx_destroy(Frame, Config).

flush() ->
    flush([]).

flush(Collected) ->
    receive Msg -> 
	    flush([Msg|Collected])
    after 1 ->
	    lists:reverse(Collected)
    end.
    	  
draw_loop(Deg,Data,Canvas) ->
    timer:sleep(15),
    {NW,NH} = wxGLCanvas:getClientSize(Canvas),
    gl:viewport(0,0,NW,NH),
    drawBox(Deg,Data),
    ?m(ok, wxGLCanvas:swapBuffers(Canvas)),
    draw_loop(Deg+1, Data,Canvas).



drawBox(Deg,{Fs,Vs}) ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:rotatef(Deg, 0.0, 1.0, 0.0),
    gl:rotatef(20, 1.0, 0.0, 1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:'begin'(?GL_QUADS),    
    lists:foreach(fun(Face) -> drawFace(Face,Vs) end, Fs),
    gl:'end'().

drawFace({{V1,V2,V3,V4},N={N1,N2,N3}}, Cube) ->
    gl:normal3fv(N),
    gl:color3f(abs(N1),abs(N2),abs(N3)),
    gl:texCoord2f(0.0, 1.0), gl:vertex3fv(element(V1, Cube)),
    gl:texCoord2f(0.0, 0.0), gl:vertex3fv(element(V2, Cube)),
    gl:texCoord2f(1.0, 0.0), gl:vertex3fv(element(V3, Cube)),
    gl:texCoord2f(1.0, 1.0), gl:vertex3fv(element(V4, Cube)).


glu_tesselation(TestInfo) when is_atom(TestInfo) -> wx_test_lib:tc_info(TestInfo);
glu_tesselation(Config) ->
    WX = ?mr(wx_ref, wx:new()),
    Frame = wxFrame:new(WX,1,"Hello 3D-World",[]),
    Attrs = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    Canvas = ?mt(wxGLCanvas, wxGLCanvas:new(Frame, Attrs)),
    wxFrame:connect(Frame, show),
    ?m(true, wxWindow:show(Frame)),

    receive #wx{event=#wxShow{}} -> ok
    after 1000 -> exit(show_timeout)
    end,
    ?m(ok, wxGLCanvas:setCurrent(Canvas)),
    
    {RL1,RB1} = ?m({_,_}, glu:tesselate({0,0,1}, [{-1,0,0},{1,0,0},{0,1,0}])),
    ?m(3, length(RL1)),
    ?m(8*3*3, size(RB1)),
    {RL2,RB2} = ?m({_,_}, glu:tesselate({0,0,1}, 
					[{-1,0,0},{1,0,0},{1,1,0},{-1,1,0}])),
    ?m(6, length(RL2)),
    ?m(8*3*4, size(RB2)),    
    {RL3,RB3} = ?m({_,_}, glu:tesselate({0,0,1}, 
					[{-1,0,0},{1,0,0},{1,1,0},
					 {-1,1,0},{-5,0.5,0}])),
    ?m(9, length(RL3)),
    ?m(8*3*5, size(RB3)),
    
    wx_test_lib:wx_destroy(Frame, Config).

    
