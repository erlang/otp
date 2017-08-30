%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-module(ex_gl).

-behaviour(wx_object).

-export([init/1, code_change/3, handle_info/2, 
	 handle_sync_event/3, handle_event/2,
	 handle_call/3, handle_cast/2, terminate/2,
	 start/1]).

-include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl"). 
-include_lib("wx/include/glu.hrl"). 

-record(state,
	{
	  parent,
	  config,
	  gl,
	  canvas,
	  image, 
	  timer,
	  time
	 }).

-record(gl, {win, data, deg, mat, alpha, text, font, brush, clock, sphere}).

-record(texture, {tid, w, h, minx, miny, maxx, maxy}).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

-define(PAD, fun(Int) -> string:right(integer_to_list(Int), 2, $0) end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Parent = proplists:get_value(parent, Config),  
    Panel = wxPanel:new(Parent, []),

    %% Setup sizer
    Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "wxGLCanvas"}]),

    Opts = [{style, ?wxFULL_REPAINT_ON_RESIZE}],
    GLAttrib = [{attribList, [?WX_GL_RGBA,
			      ?WX_GL_DOUBLEBUFFER,
			      ?WX_GL_MIN_RED,8,
			      ?WX_GL_MIN_GREEN,8,
			      ?WX_GL_MIN_BLUE,8,
			      ?WX_GL_DEPTH_SIZE,24,0]}],
    Canvas = wxGLCanvas:new(Panel,Opts ++ GLAttrib),
    wxGLCanvas:connect(Canvas, size),
    wxGLCanvas:connect(Canvas, paint, [callback]),

    Image   = wxImage:scale(wxImage:new("image.jpg"), 128,128),

    %% Add to sizers
    wxSizer:add(Sizer, Canvas, [{flag, ?wxEXPAND},{proportion, 1}]), 
    wxWindow:setSizer(Panel,Sizer),
    wxSizer:layout(Sizer),
    Timer = timer:send_interval(20, self(), update),
    {Panel, #state{parent = Panel, config = Config,
		   canvas = Canvas, image=Image,
		   timer = Timer}}.

%% Event handling
handle_sync_event(_PaintEvent, _, #state{canvas=Canvas}) ->
    %% Sync events are called from a temporary process,
    %% we need to setup the gl canvas on cocoa for some reason
    %% We do not really have to do anything, the timer event will refresh the painting
    wxGLCanvas:setCurrent(Canvas),
    DC= wxPaintDC:new(Canvas),
    wxPaintDC:destroy(DC),
    ok.

handle_event(#wx{event = #wxSize{size = {W,H}}}, State = #state{gl=GL}) ->
    if 
	GL =:= undefined ->
	    #state{canvas=Canvas, image=Image} = State,
	    wxGLCanvas:setCurrent(Canvas),
	    {noreply, State#state{gl=setup_gl(Canvas,Image)}};
	W =:= 0, H =:= 0 -> {noreply, State};
	true -> 
	    gl:viewport(0,0,W,H),
	    gl:matrixMode(?GL_PROJECTION),
	    gl:loadIdentity(),
	    gl:ortho( -2.0, 2.0, -2.0*H/W, 2.0*H/W, -20.0, 20.0),
	    gl:matrixMode(?GL_MODELVIEW),
	    gl:loadIdentity(),
	    {noreply, State}
    end.

handle_info(update, State=#state{gl=undefined}) -> 
    {noreply, State};
handle_info(update, State) ->
    S1 = update_rotation(State),
    GL = S1#state.gl,
    S2 = if S1#state.time > State#state.time ->
		 gl:deleteTextures([(GL#gl.clock)#texture.tid]),
		 {Hour,Min,Sec} = S1#state.time,
		 Clock = load_texture_by_string(GL#gl.font, GL#gl.brush, {40,40,40},
						[?PAD(Hour), $:, ?PAD(Min), $:, ?PAD(Sec)], false),
		 S1#state{gl = GL#gl{clock = Clock}};
	    true ->
		 S1
	 end,
    wx:batch(fun() -> drawBox(S2#state.gl) end),
    {noreply, S2};
handle_info(stop, State) ->
    timer:cancel(State#state.timer),
    catch wxGLCanvas:destroy(State#state.canvas),
    {stop, normal, State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    catch wxGLCanvas:destroy(State#state.canvas),
    timer:cancel(State#state.timer),
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    ok.


-define(VS, {{-0.5, -0.5, -0.5},  %1
	     { 0.5, -0.5, -0.5},  %2
	     { 0.5,  0.5, -0.5},   
	     {-0.5,  0.5, -0.5},  %4
	     {-0.5,  0.5,  0.5},
	     { 0.5,  0.5,  0.5},  %6
	     { 0.5, -0.5,  0.5}, 
	     {-0.5, -0.5,  0.5}}).%8

-define(FACES, 
	%% Faces    Normal     U-axis   V-axis 
	[{{1,2,3,4},{0,0,-1},{-1,0,0}, {0,1,0}},  % 
	 {{8,1,4,5},{-1,0,0},{0,0,1},  {0,1,0}},  %
	 {{2,7,6,3},{1,0,0}, {0,0,-1}, {0,1,0}},  %
	 {{7,8,5,6},{0,0,1}, {1,0,0},  {0,1,0}},  %
	 {{4,3,6,5},{0,1,0}, {-1,0,0}, {0,0,1}},  %
	 {{1,2,7,8},{0,-1,0},{1,0,0},  {0,0,1}}]).

-define(COLORS,{{ 0.0,  0.0,  0.0},		
		{ 1.0,  0.0,  0.0},
		{ 1.0,  1.0,  0.0}, 
		{ 0.0,  1.0,  0.0},
		{ 0.0,  1.0,  1.0},
		{ 1.0,  1.0,  1.0},
		{ 1.0,  0.0,  1.0},
		{ 0.0,  0.0,  1.0}}).


update_rotation(S=#state{gl=GL=#gl{deg=Rot}}) ->
    {_, Time} = calendar:local_time(),
    S#state{gl=GL#gl{deg = Rot + 1.0}, time = Time}.

%% Needs to setup opengl after window is shown...
%% GL context is created when shown first time.
setup_gl(Win, Image) ->
    {W,H} = wxWindow:getClientSize(Win),
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:ortho( -2.0, 2.0, -2.0*H/W, 2.0*H/W, -20.0, 20.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),    
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LESS),
    gl:clearColor(1.0,1.0,1.0,1.0),
    MatTexture = load_texture_by_image(Image),
    ImgTexture = load_texture_by_image(
		   wxImage:new("erlang.png")),
    Font = wxFont:new(32, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    Brush = wxBrush:new({0,0,0}),
    StrTexture = load_texture_by_string(Font, Brush, {40,40,40}, "Text from wxFont", true),
    {_, {Hour,Min,Sec}} = calendar:local_time(),
    Clock = load_texture_by_string(Font, Brush, {40, 40, 40},
				   [?PAD(Hour), $:, ?PAD(Min), $:, ?PAD(Sec)], false),
    Sphere = glu:newQuadric(),
    gl:enable(?GL_TEXTURE_2D),
    #gl{win=Win,data={?FACES,?VS,?COLORS},deg=0.0,
	mat=MatTexture, alpha=ImgTexture, text=StrTexture, font = Font,
	brush = Brush, clock = Clock, sphere = Sphere}.

drawBox(#gl{win=Win,deg=Deg,data={Fs,Vs,Colors},mat=MatT,alpha=ImgA,
	    text=Text, clock = Clock, sphere=Sphere}) ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:pushMatrix(),
    gl:translatef(0,0.5,0),
    gl:rotatef(Deg, 1.0, 1.0, 1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:bindTexture(?GL_TEXTURE_2D, MatT#texture.tid),
    gl:disable(?GL_BLEND), 
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),
    gl:disable(?GL_CULL_FACE),
    gl:'begin'(?GL_QUADS),
    wx:foreach(fun(Face) -> drawFace(Face,Vs,Colors) end, Fs),
    gl:'end'(),
    gl:popMatrix(),

    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),

    enter_2d_mode(Win),
    {W,H} = wxWindow:getClientSize(Win),
    Move = abs(90 - (trunc(Deg) rem 180)),
    draw_texture((W div 2) - 50, (H div 2)-130+Move, Clock),
    draw_texture((W div 2) - 80, (H div 2)-Move, ImgA),
    leave_2d_mode(),

    gl:pushMatrix(),
    gl:enable(?GL_CULL_FACE),
    gl:enable(?GL_BLEND), 
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:translatef(0,-0.8,0),
    gl:bindTexture(?GL_TEXTURE_2D, Text#texture.tid),
    glu:quadricTexture(Sphere, ?GLU_TRUE),
    glu:quadricNormals(Sphere, ?GLU_SMOOTH),
    glu:quadricDrawStyle(Sphere, ?GLU_FILL),
    glu:quadricOrientation(Sphere, ?GLU_OUTSIDE),
    %%gl:scalef(2.0, 0.5, 1.0),
    gl:rotatef(-90, 1.0, 0.0, 0.0),
    gl:rotatef(-Deg, 0.0, 0.0, 1.0),
    glu:sphere(Sphere, 0.8, 50,40),
    gl:popMatrix(),

    wxGLCanvas:swapBuffers(Win).

drawFace({{V1,V2,V3,V4},N,_Ut,_Vt}, Cube, Colors) ->
    gl:normal3fv(N),
    gl:color3fv(element(V1, Colors)),
    gl:texCoord2f(0.0, 0.0), gl:vertex3fv(element(V1, Cube)),
    gl:color3fv(element(V2, Colors)),
    gl:texCoord2f(1.0, 0.0), gl:vertex3fv(element(V2, Cube)),
    gl:color3fv(element(V3, Colors)),
    gl:texCoord2f(1.0, 1.0), gl:vertex3fv(element(V3, Cube)),
    gl:color3fv(element(V4, Colors)),
    gl:texCoord2f(0.0, 1.0), gl:vertex3fv(element(V4, Cube)).


draw_texture(X, Y,  #texture{tid = TId, w = W, h = H,
			     miny = MinY, minx = MinX,
			     maxx = MaxX, maxy = MaxY}) ->
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:'begin'(?GL_TRIANGLE_STRIP),
    gl:texCoord2f(MinX, MinY), gl:vertex2i(X,   Y  ),
    gl:texCoord2f(MaxX, MinY), gl:vertex2i(X+W div 2, Y  ),
    gl:texCoord2f(MinX, MaxY), gl:vertex2i(X,   Y+H div 2),
    gl:texCoord2f(MaxX, MaxY), gl:vertex2i(X+W div 2, Y+H div 2),
    gl:'end'().

load_texture_by_image(Image) ->
    ImgW = wxImage:getWidth(Image),
    ImgH = wxImage:getHeight(Image),
    W = get_power_of_two_roof(ImgW),
    H = get_power_of_two_roof(ImgH),
    Data = get_data_for_use_with_teximage2d(Image),
    %% Create an OpenGL texture for the image
    [TId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_NEAREST),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST),
    Format = case wxImage:hasAlpha(Image) of
		 true  -> ?GL_RGBA;
		 false -> ?GL_RGB
	     end,
    gl:texImage2D(?GL_TEXTURE_2D, 0,
		  Format, W, H, 0,
 		  Format, ?GL_UNSIGNED_BYTE, Data),
    #texture{tid = TId, w = ImgW, h = ImgH, 
	     minx = 0, miny = 0, maxx = ImgW / W, maxy = ImgH / H}.


%% This algorithm (based on http://d0t.dbclan.de/snippets/gltext.html)
%% prints a string to a bitmap and loads that onto an opengl texture.
%% Comments for the createTexture function:
%%
%%    "Creates a texture from the settings saved in TextElement, to be
%%     able to use normal system fonts conviently a wx.MemoryDC is
%%     used to draw on a wx.Bitmap. As wxwidgets device contexts don't
%%     support alpha at all it is necessary to apply a little hack to
%%     preserve antialiasing without sticking to a fixed background
%%     color:
%%
%%     We draw the bmp in b/w mode so we can use its data as a alpha
%%     channel for a solid color bitmap which after GL_ALPHA_TEST and
%%     GL_BLEND will show a nicely antialiased text on any surface.
%% 
%%     To access the raw pixel data the bmp gets converted to a
%%     wx.Image. Now we just have to merge our foreground color with
%%     the alpha data we just created and push it all into a OpenGL
%%     texture and we are DONE *inhalesdelpy*"
load_texture_by_string(Font, Brush, Color, String, Flip) ->
    TmpBmp = wxBitmap:new(200, 200),
    Tmp = wxMemoryDC:new(TmpBmp),
    wxMemoryDC:setFont(Tmp, Font),        
    {StrW, StrH} = wxDC:getTextExtent(Tmp, String),
    wxMemoryDC:destroy(Tmp),
    wxBitmap:destroy(TmpBmp),
    
    W = get_power_of_two_roof(StrW),
    H = get_power_of_two_roof(StrH),

    Bmp = wxBitmap:new(W, H),
    DC = wxMemoryDC:new(Bmp),
    wxMemoryDC:setFont(DC, Font),        
    wxMemoryDC:setBackground(DC, Brush),
    wxMemoryDC:clear(DC),
    wxMemoryDC:setTextForeground(DC, {255, 255, 255}),
    wxMemoryDC:drawText(DC, String, {0, 0}),

    Img0 = wxBitmap:convertToImage(Bmp),
    case Flip of
	true ->
	    Img = wxImage:mirror(Img0, [{horizontally, false}]),
	    wxImage:destroy(Img0),
	    Img;
	false ->
	    Img = Img0
    end,
    
    Alpha = wxImage:getData(Img),
    Data = colourize_image(Alpha, Color),
    wxImage:destroy(Img),
    wxBitmap:destroy(Bmp),
    wxMemoryDC:destroy(DC),

    [TId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    %%gl:pixelStorei(?GL_UNPACK_ROW_LENGTH, 0),
    %%gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 2),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA,
  		  W, H, 0, ?GL_RGBA, ?GL_UNSIGNED_BYTE, Data),
    #texture{tid = TId, w = StrW, h = StrH, 
 	     minx = 0, miny = 0, maxx = StrW / W, maxy = StrH / H}.

colourize_image(Alpha, {R,G,B}) ->
    << <<R:8,G:8,B:8,A:8>> || <<A:8,_:8,_:8>> <= Alpha >>.

get_data_for_use_with_teximage2d(Image) ->
    RGB = wxImage:getData(Image),
    case wxImage:hasAlpha(Image) of
	true ->
 	    Alpha = wxImage:getAlpha(Image),
 	    interleave_rgb_and_alpha(RGB, Alpha);
	false ->
	    RGB
    end.

interleave_rgb_and_alpha(RGB, Alpha) ->
    list_to_binary(
      lists:zipwith(fun({R, G, B}, A) ->
			    <<R, G, B, A>>
				end,
		    [{R,G,B} || <<R, G, B>> <= RGB],
		    [A || <<A>> <= Alpha])).


enter_2d_mode(Win) ->
    {W, H} = wxWindow:getClientSize(Win),

    %% Note, there may be other things you need to change,
    %% depending on how you have your OpenGL state set up.
    gl:pushAttrib(?GL_ENABLE_BIT),
    gl:disable(?GL_DEPTH_TEST),
    gl:disable(?GL_CULL_FACE),
    gl:enable(?GL_TEXTURE_2D),

    %% This allows alpha blending of 2D textures with the scene
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),

    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),

    %% SDL coordinates will be upside-down in the OpenGL world.  We'll
    %% therefore flip the bottom and top coordinates in the orthogonal
    %% projection to correct this.  
    %% Note: We could flip the texture/image itself, but this will
    %% also work for mouse coordinates.
    gl:ortho(0.0, W, H, 0.0, 0.0, 1.0),

    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity().

leave_2d_mode() ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:popAttrib().

get_power_of_two_roof(X) ->
    get_power_of_two_roof_2(1, X).

get_power_of_two_roof_2(N, X) when N >= X -> N;
get_power_of_two_roof_2(N, X) -> get_power_of_two_roof_2(N*2, X).

