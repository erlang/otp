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
-module(canvasbutton).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Erlang Toolbar
%
%%% Description %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extension to GS used to imitate buttons but instead using images drawn
% on a canvas. Enables usage of .gif files as button images and not only
% .xbm (bitmap) files.
%
%%% Constants %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
-define(gray,{87,87,87}).
%
%%% Internal data structures %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% canvasbutton()
-record(canvasbutton,{image,rect,ul,ll}).
%
% cboptions()
-record(cboptions,
	{imagefile=nofile, % nofile |
	                   % string()   Name of image file
	 x=0,              % integer()  X coordinate relative the canvas
	 y=0,              % integer()  Y coordinate relative the canvas
	 width=10,         % integer()  Button width
	 height=10,        % integer()  Button heigth
	 fg=black,         % atom()     Foreground color
	 data=[]}).        % term()     Data associated with button events
%
%%% Exports %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
-export([create/1,create/2,read/2,press/1,release/1]).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Exported functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------
% create(Canvas) => create(Canvas,[])
% create(Canvas,OptionList) => canvasbutton()
%   Canvas - GS canvas object
%   OptionList - [{Option,Value}]
%   Option, Value - see below
% Create a canvasbutton with the given image on Canvas
%
%   Option     Value      Default  Comment
%   ----------------------------------------------------------------
%   image    nofile |    nofile   Name of image file. Must be a bitmap
%            string()             file (.xbm) or a GIF file (.gif).
%   x        integer()   0        X coordinate relative to Canvas
%   y        integer()   0        Y coordinate relative to Canvas
%   width    integer()   10       Button width
%   height   integer()   10       Button height
%   fg       atom()      black    Foreground color, bitmaps only
%   data     term()      []       Data associated with button events
%
% The process calling this function will receive the following events:
%   {gs,GsObj,enter,{canvasbutton,Canvasbutton,Data},Args}
%   {gs,GsObj,leave,{canvasbutton,Canvasbutton,Data},Args}
%   {gs,GsObj,buttonpress,{canvasbutton,Canvasbutton,Data},Args}
%   {gs,GsObj,buttonrelease,{canvasbutton,Canvasbutton,Data},Args}
% where GsObj and Args are a GS object and its Args field, respectively.
%
% Note that press/1 and release/1 has to be called explicitly to create
% the illusion of the button being pressed or released.
%----------------------------------------
create(Canvas) ->
    create(Canvas,[]).
create(Canvas,OptionList) ->
    Options = sort_out_options(OptionList),
    X = Options#cboptions.x,
    Y = Options#cboptions.y,
    W = Options#cboptions.width,
    H = Options#cboptions.height,

    %% Buttons should have the same background color as the canvas
    Bg = gs:read(Canvas,bc),

    %% Draw image
    Image = create_image(Options#cboptions.imagefile,Canvas,Bg,
			 Options#cboptions.fg,X,Y,W,H),

    %% Draw upper left corner line
    Ul = gs:create(line,Canvas,[{coords,[{X,Y+H},{X,Y},{X+W,Y}]},
				{fg,white},{width,2}]),

    %% Draw lower right corner line
    Ll = gs:create(line,Canvas,[{coords,[{X,Y+H},{X+W,Y+H},{X+W,Y}]},
				{fg,?gray},{width,2}]),
				

    %% Draw a rectangle around all (for event catching when width and
    %% height of button is larger than image)
    Rect = gs:create(rectangle,Canvas,[{coords,[{X,Y},{X+W,Y+H}]},
				       {fill,Bg},
				       {buttonpress,true},
				       {buttonrelease,true},
				       {enter,true},{leave,true}]),

    %% Now the canvas button is created
    Canvasbutton = #canvasbutton{image=Image,rect=Rect,ul=Ul,ll=Ll},

    Data = Options#cboptions.data,
    gs:config(Rect,{data,{canvasbutton,Canvasbutton,Data}}),
    gs:config(Image,{data,{canvasbutton,Canvasbutton,Data}}),
    gs:config(Rect,lower),
    gs:config(Image,raise),

    Canvasbutton.

%----------------------------------------
% read(Canvasbutton,coords) => [{L,T},{R,B}]
%   Canvasbutton - canvasbutton()
% Read a Canvasbutton's coordinates
%----------------------------------------
read(Canvasbutton,coords) ->
    gs:read(Canvasbutton#canvasbutton.rect,coords).

%----------------------------------------
% press(Canvasbutton)
%   Canvasbutton - canvasbutton()
% Create the illusion that Canvasbutton is pressed
%----------------------------------------
press(Canvasbutton) ->
    gs:config(Canvasbutton#canvasbutton.ul,{fg,?gray}),
    gs:config(Canvasbutton#canvasbutton.ll,{fg,white}),
    case Canvasbutton#canvasbutton.image of
	noimage ->
	    ignore;
	Image ->
	    gs:config(Image,{move,{-1,-1}})
    end.

%----------------------------------------
% release(Canvasbutton)
%   Canvasbutton - canvasbutton()
% Create the illusion that Canvasbutton is released
%----------------------------------------
release(Canvasbutton) ->
    gs:config(Canvasbutton#canvasbutton.ul,{fg,white}),
    gs:config(Canvasbutton#canvasbutton.ll,{fg,?gray}),
    case Canvasbutton#canvasbutton.image of
	noimage ->
	    ignore;
	Image ->
	    gs:config(Image,{move,{1,1}})
    end.


%%% Internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------
% create_image(ImageFile,Canvas,Bg,Fg,X,Y,W,H) => Image
%   ImageFile - string() Image file, must exist and be a bitmap file ending
%               with .xbm or a GIF file ending with .gif.
%   Canvas - GS canvas object
%   Bg - atom() Background color (bitmaps only)
%   Fg - atom() Foreground color (bitmaps only)
%   X, Y - int() X and Y coordinates for Image relative to Canvas
%   W, H - int() Width and height of Image
%   Image - GS canvas image object
%----------------------------------------
create_image(nofile,_,_,_,_,_,_,_) ->
    noimage;
create_image(ImageFile,Canvas,Bg,Fg,X,Y,W,H) ->
    case lists:last(string:tokens(ImageFile,".")) of
	"xbm" ->
	    gs:create(image,Canvas,
		      [{bitmap,ImageFile},
		       {bg,Bg},{fg,Fg},
		       {anchor,c},
		       {coords,[{X+1+W/2,Y+1+H/2}]},
		       {buttonpress,true},
		       {buttonrelease,true},
		       {enter,true},{leave,true}]);
	"gif" ->
	    gs:create(image,Canvas,
		      [{load_gif,ImageFile},
		       {anchor,c},
		       {coords,[{X+W/2,Y+H/2}]},
		       {buttonpress,true},
		       {buttonrelease,true},
		       {enter,true},{leave,true}])
    end.

%----------------------------------------
% sort_out_options(OptionList) => cboptions()
%   OptionList - see create/2
% Insert members of option list into a cboptions record.
%----------------------------------------
sort_out_options(OptionList) ->
    sort_out_options(OptionList,#cboptions{}).

%----------------------------------------
% sort_out_options(OptionList,Options) => cboptions()
%   OptionList - see create/2
%   Options - cboptions()
% Called by sort_out_options/1.
%----------------------------------------
sort_out_options([{image,Image}|Rest],Options) ->
    sort_out_options(Rest,Options#cboptions{imagefile=Image});
sort_out_options([{x,X}|Rest],Options) ->
    sort_out_options(Rest,Options#cboptions{x=X});
sort_out_options([{y,Y}|Rest],Options) ->
    sort_out_options(Rest,Options#cboptions{y=Y});
sort_out_options([{width,Width}|Rest],Options) ->
    sort_out_options(Rest,Options#cboptions{width=Width});
sort_out_options([{height,Height}|Rest],Options) ->
    sort_out_options(Rest,Options#cboptions{height=Height});
sort_out_options([{fg,Fg}|Rest],Options) ->
    sort_out_options(Rest,Options#cboptions{fg=Fg});
sort_out_options([{data,Data}|Rest],Options) ->
    sort_out_options(Rest,Options#cboptions{data=Data});
sort_out_options([],Options) ->
    Options.
