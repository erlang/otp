%% -*- erlang-indent-level: 2 -*-
%%==========================================================================
%% From: Tomas Stejskal -- 23/02/2008
%% I've found some strange behavior regarding binary matching. The module's
%% purpose is reading an id3 version 1 or version 1.1 tag from an mp3 bin.
%% When I use the function read_v1_or_v11_tag on a mp3 binary containing
%% version 1 tag, it returns an error. However, when the function
%% read_only_v1_tag is applied on the same file, it reads the tag data
%% correctly. The only difference between these two functions is that the
%% former has an extra branch to read version 1.1 tag.
%% This was a BEAM compiler bug which was fixed by a patch to beam_dead.
%%==========================================================================

-module(bs_id3).

-export([test/0]).

-define(BIN, <<84,65,71,68,117,154,105,232,107,121,0,0,0,0,0,0,0,0,0,0,0,
	       0,0,0,0,0,0,0,0,0,0,0,0,68,97,110,105,101,108,32,76,97,110,
	       100,97,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,66,101,115,116,
	       32,79,102,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	       50,48,48,48,50,48,48,48,32,45,32,66,101,115,116,32,79,102,
	       32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,12>>).

test() ->
  R1 = parse_only_v1_tag(?BIN),
  R2 = parse_v1_or_v11_tag(?BIN),
  %% io:format("~p\n~p\n", [R1, R2]),
  R1 = R2,	% crash if not equal
  ok.

parse_only_v1_tag(<<"TAG", Title:30/binary,
		    Artist:30/binary, Album:30/binary,
	            _Year:4/binary, _Comment:30/binary,
		    _Genre:8>>) ->
  {ok,
   {"ID3v1",
    [{title, trim(Title)},
     {artist, trim(Artist)},
     {album, trim(Album)}]}};
parse_only_v1_tag(_) ->
  error.

parse_v1_or_v11_tag(<<"TAG", Title:30/binary,
		      Artist:30/binary, Album:30/binary,
		      _Year:4/binary, _Comment:28/binary,
		      0:8, Track:8, _Genre:8>>) ->
  {ok,
   {"ID3v1.1",
    [{track, Track}, {title, trim(Title)},
     {artist, trim(Artist)}, {album, trim(Album)}]}};
parse_v1_or_v11_tag(<<"TAG", Title:30/binary,
	              Artist:30/binary, Album:30/binary,
	              _Year:4/binary, _Comment:30/binary,
	              _Genre:8>>) ->
  {ok,
   {"ID3v1",
    [{title, trim(Title)},
     {artist, trim(Artist)},
     {album, trim(Album)}]}};
parse_v1_or_v11_tag(_) ->
  error.

trim(Bin) ->
  list_to_binary(trim_blanks(binary_to_list(Bin))).

trim_blanks(L) ->
  lists:reverse(skip_blanks_and_zero(lists:reverse(L))).

skip_blanks_and_zero([$\s|T]) ->
  skip_blanks_and_zero(T);
skip_blanks_and_zero([0|T]) ->
  skip_blanks_and_zero(T);
skip_blanks_and_zero(L) ->
  L.
