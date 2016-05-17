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

%% 
%% @doc egd_font 
%%

-module(egd_font).

-export([load/1, size/1, glyph/2]).
-include("egd.hrl").

%% Font represenatation in ets table
%% egd_font_table
%%
%% Information:
%% {Key, Description, Size}
%%	Key :: {Font :: atom(), information}
%%	Description :: any(), Description header from font file 
%%	Size :: {W :: integer(), H :: integer()}
%%
%% Glyphs:
%% {Key, Translation LSs} where
%%	Key :: {Font :: atom(), Code :: integer()}, Code = glyph char code
%%	Translation :: {
%%		W  :: integer(), % BBx width
%%		H  :: integer(), % BBx height
%%		X0 :: integer(), % X start
%%		Y0 :: integer(), % Y start
%%		Xm :: integer(), % Glyph X move when drawing
%%	    }
%%	LSs :: [[{Xl :: integer(), Xr :: integer()}]]
%%	The first list is height (top to bottom), the inner list is the list
%%	of line spans for the glyphs horizontal pixels.
%%

%%==========================================================================
%% Interface functions
%%==========================================================================

size(Font) ->
    [{_Key, _Description, Size}] = ets:lookup(egd_font_table,{Font,information}),
    Size.

glyph(Font, Code) ->
    [{_Key, Translation, LSs}] = ets:lookup(egd_font_table,{Font,Code}),
    {Translation, LSs}.

load(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Font = erlang:binary_to_term(Bin),
    load_font_header(Font).

%%==========================================================================
%% Internal functions
%%==========================================================================

%% ETS handler functions

initialize_table() ->
    egd_font_table = ets:new(egd_font_table, [named_table, ordered_set, public]),
    ok.

glyph_insert(Font, Code, Translation, LSs) ->
    Element = {{Font, Code}, Translation, LSs},
    ets:insert(egd_font_table, Element).

font_insert(Font, Description, Dimensions) ->
    Element = {{Font, information}, Description, Dimensions},
    ets:insert(egd_font_table, Element).

%% Font loader functions

is_font_loaded(Font) ->
    try
        case ets:lookup(egd_font_table, {Font, information}) of 
  	    [] -> false;
	    _ -> true
        end
    catch
	error:_ ->
	    initialize_table(),
	    false
    end.
    

load_font_header({_Type, _Version, Font}) ->
    load_font_body(Font).
    
load_font_body({Key,Desc,W,H,Glyphs,Bitmaps}) ->
    case is_font_loaded(Key) of
	true  -> Key;
	false ->
	    % insert dimensions
	    font_insert(Key, Desc, {W,H}),
	    parse_glyphs(Glyphs, Bitmaps, Key),
	    Key
    end.

parse_glyphs([], _ , _Key) -> ok;
parse_glyphs([Glyph|Glyphs], Bs, Key) ->
    {Code, Translation, LSs} = parse_glyph(Glyph, Bs),
    glyph_insert(Key, Code, Translation, LSs),
    parse_glyphs(Glyphs, Bs, Key).

parse_glyph({Code,W,H,X0,Y0,Xm,Offset}, Bitmasks) ->
    BytesPerLine = ((W+7) div 8),
    NumBytes = BytesPerLine*H,
    <<_:Offset/binary,Bitmask:NumBytes/binary,_/binary>> = Bitmasks,
    LSs = render_glyph(W,H,X0,Y0,Xm,Bitmask),
    {Code, {W,H,X0,Y0,Xm}, LSs}.

render_glyph(W, H, X0, Y0, Xm, Bitmask) ->
    render_glyph(W,{0,H},X0,Y0,Xm,Bitmask, []).
render_glyph(_W, {H,H}, _X0, _Y0, _Xm, _Bitmask, Out) -> Out;
render_glyph(W, {Hi,H}, X0, Y0,Xm, Bitmask , LSs) ->
    N = ((W+7) div 8),
    O = N*Hi,
    <<_:O/binary, Submask/binary>> = Bitmask,
     LS = render_glyph_horizontal(
	Submask,         % line glyph bitmask
	{down, W - 1},   % loop state
	W - 1,           % Width
	[]),             % Linespans
    render_glyph(W,{Hi+1,H},X0,Y0,Xm, Bitmask, [LS|LSs]).

render_glyph_horizontal(Value, {Pr, Px}, 0, Spans) ->
    Cr = bit_spin(Value, 0),
    case {Pr,Cr} of
	{up  , up  } -> % closure of interval since its last
	    [{0, Px}|Spans];
	{up  , down} -> % closure of interval
	    [{1, Px}|Spans];
	{down, up  } -> % beginning of interval
	    [{0,  0}|Spans];
	{down, down} -> % no change in interval
	    Spans
    end;
render_glyph_horizontal(Value, {Pr, Px}, Cx, Spans) ->
    Cr = bit_spin(Value, Cx),
    case {Pr,Cr} of
	{up  , up  } -> % no change in interval
	    render_glyph_horizontal(Value, {Cr, Px}, Cx - 1, Spans);
	{up  , down} -> % closure of interval
	    render_glyph_horizontal(Value, {Cr, Cx}, Cx - 1, [{Cx+1,Px}|Spans]);
	{down, up  } -> % beginning of interval
	    render_glyph_horizontal(Value, {Cr, Cx}, Cx - 1, Spans);
	{down, down} -> % no change in interval
	    render_glyph_horizontal(Value, {Cr, Px}, Cx - 1, Spans)
    end.

bit_spin(Value, Cx) ->
    <<_:Cx, Bit:1, _/bits>> = Value,
    case Bit of
	1 -> up;
	0 -> down
    end.
