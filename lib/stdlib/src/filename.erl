%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(filename).

%% Purpose: Provides generic manipulation of filenames.
%%
%% Generally, these functions accept filenames in the native format
%% for the current operating system (Unix or Windows).
%% Deep characters lists (as returned by io_lib:format()) are accepted;
%% resulting strings will always be flat.
%%
%% Implementation note: We used to only flatten if the list turned out
%% to be deep. Now that atoms are allowed in deep lists, in most cases
%% we flatten the arguments immediately on function entry as that makes
%% it easier to ensure that the code works.

-export([absname/1, absname/2, absname_join/2, 
	 basename/1, basename/2, dirname/1,
	 extension/1, join/1, join/2, pathtype/1,
         rootname/1, rootname/2, split/1, nativename/1,
         safe_relative_path/1]).
-export([find_src/1, find_src/2, flatten/1]).
-export([basedir/2, basedir/3]).

%% Undocumented and unsupported exports.
-export([append/2]).

-include_lib("kernel/include/file.hrl").

-define(IS_DRIVELETTER(Letter),(((Letter >= $A) andalso (Letter =< $Z)) orelse
				((Letter >= $a) andalso (Letter =< $z)))). 

%% Converts a relative filename to an absolute filename
%% or the filename itself if it already is an absolute filename
%% Note that no attempt is made to create the most beatiful
%% absolute name since this can give incorrect results on 
%% file systems which allows links.
%% Examples:
%% Assume (for UNIX) current directory "/usr/local"
%% Assume (for WIN32) current directory "D:/usr/local"
%%  
%% (for Unix) : absname("foo") -> "/usr/local/foo"
%% (for WIN32): absname("foo") -> "D:/usr/local/foo"
%% (for Unix) : absname("../x") -> "/usr/local/../x"
%% (for WIN32): absname("../x") -> "D:/usr/local/../x"
%% (for Unix) : absname("/") -> "/"
%% (for WIN32): absname("/") -> "D:/"


-spec absname(Filename) -> file:filename_all() when
      Filename :: file:name_all().
absname(Name) ->
    {ok, Cwd} = file:get_cwd(),
    absname(Name, Cwd).

-spec absname(Filename, Dir) -> file:filename_all() when
      Filename :: file:name_all(),
      Dir :: file:name_all().
absname(Name, AbsBase) when is_binary(Name), is_list(AbsBase) ->
    absname(Name,filename_string_to_binary(AbsBase));
absname(Name, AbsBase) when is_list(Name), is_binary(AbsBase) ->
    absname(filename_string_to_binary(Name),AbsBase);

absname(Name, AbsBase) ->
    case pathtype(Name) of
	relative ->
	    absname_join(AbsBase, Name);
	absolute ->
	    %% We must flatten the filename before passing it into join/1,
	    %% or we will get slashes inserted into the wrong places.
	    join([flatten(Name)]);
	volumerelative ->
	    absname_vr(split(Name), split(AbsBase), AbsBase)
    end.

%% Handles volumerelative names (on Windows only).

absname_vr([<<"/">>|Rest1], [Volume|_], _AbsBase) ->
    %% Absolute path on current drive.
    join([Volume|Rest1]);
absname_vr([<<X, $:>>|Rest1], [<<X,_/binary>>|_], AbsBase) ->
    %% Relative to current directory on current drive.
    absname(join(Rest1), AbsBase);
absname_vr([<<X, $:>>|Name], _, _AbsBase) ->
    %% Relative to current directory on another drive.
    Dcwd =
	case file:get_cwd([X, $:]) of
	    {ok, Dir}  -> filename_string_to_binary(Dir);
	    {error, _} -> <<X, $:, $/>>
    end,
    absname(join(Name), Dcwd);
absname_vr(["/"|Rest1], [Volume|_], _AbsBase) ->
    %% Absolute path on current drive.
    join([Volume|Rest1]);
absname_vr([[X, $:]|Rest1], [[X|_]|_], AbsBase) ->
    %% Relative to current directory on current drive.
    absname(join(Rest1), AbsBase);
absname_vr([[X, $:]|Name], _, _AbsBase) ->
    %% Relative to current directory on another drive.
    Dcwd =
	case file:get_cwd([X, $:]) of
	    {ok, Dir}  -> Dir;
	    {error, _} -> [X, $:, $/]
    end,
    absname(join(Name), Dcwd).

%% Joins a relative filename to an absolute base. 
%% This is just a join/2, but assumes that 
%% AbsBase must be absolute and Name must be relative.

-spec absname_join(Dir, Filename) -> file:filename_all() when
      Dir :: file:name_all(),
      Filename :: file:name_all().
absname_join(AbsBase, Name) ->
    join(AbsBase, flatten(Name)).

%% Returns the part of the filename after the last directory separator,
%% or the filename itself if it has no separators.
%%
%% Examples: basename("foo") -> "foo"
%%           basename("/usr/foo") -> "foo"
%%           basename("/usr/foo/") -> "foo"  (trailing slashes ignored)
%%           basename("/") -> []

-spec basename(Filename) -> file:filename_all() when
      Filename :: file:name_all().

basename(Name) when is_binary(Name) ->
    case os:type() of
	{win32,_} ->
	    win_basenameb(Name);
	_ ->
	    basenameb(Name,[<<"/">>])
    end;
    
basename(Name0) ->
    Name1 = flatten(Name0),
    {DirSep2, DrvSep} = separators(),
    Name = skip_prefix(Name1, DrvSep),
    basename1(Name, Name, DirSep2).

win_basenameb(<<Letter,$:,Rest/binary>>) when ?IS_DRIVELETTER(Letter) ->
    basenameb(Rest,[<<"/">>,<<"\\">>]);
win_basenameb(O) ->
    basenameb(O,[<<"/">>,<<"\\">>]).
basenameb(Bin,Sep) ->
    Parts = [ X || X <- binary:split(Bin,Sep,[global]),
		   X =/= <<>> ],
    if
	Parts =:= [] ->
	    <<>>;
	true ->
	    lists:last(Parts)
    end.
    


basename1([$/], Tail0, _DirSep2) ->
    %% End of filename -- must get rid of trailing directory separator.
    [_|Tail] = lists:reverse(Tail0),
    lists:reverse(Tail);
basename1([$/|Rest], _Tail, DirSep2) ->
    basename1(Rest, Rest, DirSep2);
basename1([DirSep2|Rest], Tail, DirSep2) when is_integer(DirSep2) ->
    basename1([$/|Rest], Tail, DirSep2);
basename1([Char|Rest], Tail, DirSep2) when is_integer(Char) ->
    basename1(Rest, Tail, DirSep2);
basename1([], Tail, _DirSep2) ->
    Tail.

skip_prefix(Name, false) ->
    Name;
skip_prefix([L, DrvSep|Name], DrvSep) when ?IS_DRIVELETTER(L) ->
    Name;
skip_prefix(Name, _) ->
    Name.

%% Returns the last component of the filename, with the given
%% extension stripped.  Use this function if you want
%% to remove an extension that might or might not be there.
%% Use rootname(basename(File)) if you want to remove an extension
%% that you know exists, but you are not sure which one it is.
%%
%% Example: basename("~/src/kalle.erl", ".erl") -> "kalle"
%%	    basename("~/src/kalle.jam", ".erl") -> "kalle.jam"
%%	    basename("~/src/kalle.old.erl", ".erl") -> "kalle.old"
%%
%%	    rootname(basename("xxx.jam")) -> "xxx"
%%	    rootname(basename("xxx.erl")) -> "xxx"

-spec basename(Filename, Ext) -> file:filename_all() when
      Filename :: file:name_all(),
      Ext :: file:name_all().
basename(Name, Ext) when is_binary(Name), is_list(Ext) ->
    basename(Name,filename_string_to_binary(Ext));
basename(Name, Ext) when is_list(Name), is_binary(Ext) ->
    basename(filename_string_to_binary(Name),Ext);
basename(Name, Ext) when is_binary(Name), is_binary(Ext) ->
    BName = basename(Name),
    LAll = byte_size(Name),
    LN = byte_size(BName),
    LE = byte_size(Ext),
    case LN - LE of
	Neg when Neg < 0 ->
	    BName;
	Pos ->
	    StartLen = LAll - Pos - LE,
	    case Name of
		<<_:StartLen/binary,Part:Pos/binary,Ext/binary>> ->
		    Part;
		_Other ->
		    BName
	    end
    end;

basename(Name0, Ext0) ->
    Name = flatten(Name0),
    Ext = flatten(Ext0),
    {DirSep2,DrvSep} = separators(),
    NoPrefix = skip_prefix(Name, DrvSep),
    basename(NoPrefix, Ext, [], DirSep2).

basename(Ext, Ext, Tail, _DrvSep2) ->
    lists:reverse(Tail);
basename([$/|[]], Ext, Tail, DrvSep2) ->
    basename([], Ext, Tail, DrvSep2);
basename([$/|Rest], Ext, _Tail, DrvSep2) ->
    basename(Rest, Ext, [], DrvSep2);
basename([DirSep2|Rest], Ext, Tail, DirSep2) when is_integer(DirSep2) ->
    basename([$/|Rest], Ext, Tail, DirSep2);
basename([Char|Rest], Ext, Tail, DrvSep2) when is_integer(Char) ->
    basename(Rest, Ext, [Char|Tail], DrvSep2);
basename([], _Ext, Tail, _DrvSep2) ->
    lists:reverse(Tail).

%% Returns the directory part of a pathname.
%%
%% Example: dirname("/usr/src/kalle.erl") -> "/usr/src",
%%	    dirname("kalle.erl") -> "."

-spec dirname(Filename) -> file:filename_all() when
      Filename :: file:name_all().
dirname(Name) when is_binary(Name) ->
    {Dsep,Drivesep} = separators(),
    SList = case Dsep of
		Sep when is_integer(Sep) -> 
		    [ <<Sep>> ];
		_ ->
		    []
	    end,
    {XPart0,Dirs} = case Drivesep of
		       X when is_integer(X) ->
			   case Name of
			       <<DL,X,Rest/binary>> when ?IS_DRIVELETTER(DL) ->
				   {<<DL,X>>,Rest};
			       _ ->
				   {<<>>,Name}
			   end;
		       _ ->
			   {<<>>,Name} 
		   end,
    Parts0 = binary:split(Dirs,[<<"/">>|SList],[global]),
    %% Fairly short lists of parts, OK to reverse twice...
    Parts = case Parts0 of
		[] -> [];
		_ -> lists:reverse(fstrip(tl(lists:reverse(Parts0))))
	    end,
    XPart = case {Parts,XPart0} of
		{[],<<>>} ->
		    <<".">>;
		_ ->
		    XPart0
	    end,
    dirjoin(Parts,XPart,<<"/">>);

dirname(Name0) ->
    Name = flatten(Name0),
    dirname(Name, [], [], separators()).

dirname([$/|Rest], Dir, File, Seps) ->
    dirname(Rest, File++Dir, [$/], Seps);
dirname([DirSep|Rest], Dir, File, {DirSep,_}=Seps) when is_integer(DirSep) ->
    dirname(Rest, File++Dir, [$/], Seps);
dirname([Dl,DrvSep|Rest], [], [], {_,DrvSep}=Seps)
  when is_integer(DrvSep), ((($a =< Dl) and (Dl =< $z)) or
			    (($A =< Dl) and (Dl =< $Z))) ->
    dirname(Rest, [DrvSep,Dl], [], Seps);
dirname([Char|Rest], Dir, File, Seps) when is_integer(Char) ->
    dirname(Rest, Dir, [Char|File], Seps);
dirname([], [], File, _Seps) ->
    case lists:reverse(File) of
	[$/|_] -> [$/];
	_ -> "."
    end;
dirname([], [$/|Rest], File, Seps) ->
    dirname([], Rest, File, Seps);
dirname([], [DrvSep,Dl], File, {_,DrvSep}) ->
    case lists:reverse(File) of
	[$/|_] -> [Dl,DrvSep,$/];
	_ -> [Dl,DrvSep]
    end;
dirname([], Dir, _, _) ->
    lists:reverse(Dir).

%% Compatibility with lists variant, remove trailing slashes
fstrip([<<>>,X|Y]) ->
    fstrip([X|Y]);
fstrip(A) ->
    A.
			   

dirjoin([<<>>|T],Acc,Sep) ->
    dirjoin1(T,<<Acc/binary,"/">>,Sep);
dirjoin(A,B,C) ->
    dirjoin1(A,B,C).

dirjoin1([],Acc,_) ->
    Acc;
dirjoin1([One],Acc,_) ->
    <<Acc/binary,One/binary>>;
dirjoin1([H|T],Acc,Sep) ->
    dirjoin(T,<<Acc/binary,H/binary,Sep/binary>>,Sep).

    
%% Given a filename string, returns the file extension,
%% including the period.  Returns an empty list if there
%% is no extension.
%%
%% Example: extension("foo.erl") -> ".erl"
%%	    extension("jam.src/kalle") -> ""
%%
%% On Windows:  fn:dirname("\\usr\\src/kalle.erl") -> "/usr/src"

-spec extension(Filename) -> file:filename_all() when
      Filename :: file:name_all().
extension(Name) when is_binary(Name) ->
    {Dsep,_} = separators(),
    SList = case Dsep of
		Sep when is_integer(Sep) -> 
		    [ <<Sep>> ];
		_ ->
		    []
	    end,
    case binary:matches(Name,[<<".">>]) of
	[] ->
	    <<>>;
	List ->
	    {Pos,_} = lists:last(List),
	    <<_:Pos/binary,Part/binary>> = Name,
	    case binary:match(Part,[<<"/">>|SList]) of
		nomatch ->
		    Part;
		_ ->
		    <<>>
	    end
    end;

extension(Name0) ->
    Name = flatten(Name0),
    extension(Name, [], major_os_type()).

extension([$.|Rest]=Result, _Result, OsType) ->
    extension(Rest, Result, OsType);
extension([Char|Rest], [], OsType) when is_integer(Char) ->
    extension(Rest, [], OsType);
extension([$/|Rest], _Result, OsType) ->
    extension(Rest, [], OsType);
extension([$\\|Rest], _Result, win32) ->
    extension(Rest, [], win32);
extension([Char|Rest], Result, OsType) when is_integer(Char) ->
    extension(Rest, Result, OsType);
extension([], Result, _OsType) ->
    Result.

%% Joins a list of filenames with directory separators.

-spec join(Components) -> file:filename_all() when
      Components :: [file:name_all()].
join([Name1, Name2|Rest]) ->
    join([join(Name1, Name2)|Rest]);
join([Name]) when is_list(Name) ->
    join1(Name, [], [], major_os_type());
join([Name]) when is_binary(Name) ->
    join1b(Name, <<>>, [], major_os_type());
join([Name]) when is_atom(Name) ->
    join([atom_to_list(Name)]).

%% Joins two filenames with directory separators.

-spec join(Name1, Name2) -> file:filename_all() when
      Name1 :: file:name_all(),
      Name2 :: file:name_all().
join(Name1, Name2) when is_list(Name1), is_list(Name2) ->
    OsType = major_os_type(),
    case pathtype(Name2) of
	relative -> join1(Name1, Name2, [], OsType);
	_Other -> join1(Name2, [], [], OsType)
    end;
join(Name1, Name2) when is_binary(Name1), is_list(Name2) ->
    join(Name1,filename_string_to_binary(Name2));
join(Name1, Name2) when is_list(Name1), is_binary(Name2) ->
    join(filename_string_to_binary(Name1),Name2);
join(Name1, Name2) when is_binary(Name1), is_binary(Name2) ->
    OsType = major_os_type(),
    case pathtype(Name2) of
	relative -> join1b(Name1, Name2, [], OsType);
	_Other -> join1b(Name2, <<>>, [], OsType)
    end;
    
join(Name1, Name2) when is_atom(Name1) ->
    join(atom_to_list(Name1), Name2);
join(Name1, Name2) when is_atom(Name2) ->
    join(Name1, atom_to_list(Name2)).

%% Internal function to join an absolute name and a relative name.
%% It is the responsibility of the caller to ensure that RelativeName
%% is relative.

join1([UcLetter, $:|Rest], RelativeName, [], win32)
when is_integer(UcLetter), UcLetter >= $A, UcLetter =< $Z ->
    join1(Rest, RelativeName, [$:, UcLetter+$a-$A], win32);
join1([$\\|Rest], RelativeName, Result, win32) ->
    join1([$/|Rest], RelativeName, Result, win32);
join1([$/|Rest], RelativeName, [$., $/|Result], OsType) ->
    join1(Rest, RelativeName, [$/|Result], OsType);
join1([$/|Rest], RelativeName, [$/|Result], OsType) ->
    join1(Rest, RelativeName, [$/|Result], OsType);
join1([], [], Result, OsType) ->
    maybe_remove_dirsep(Result, OsType);
join1([], RelativeName, [$:|Rest], win32) ->
    join1(RelativeName, [], [$:|Rest], win32);
join1([], RelativeName, [$/|Result], OsType) ->
    join1(RelativeName, [], [$/|Result], OsType);
join1([], RelativeName, [$., $/|Result], OsType) ->
    join1(RelativeName, [], [$/|Result], OsType);
join1([], RelativeName, Result, OsType) ->
    join1(RelativeName, [], [$/|Result], OsType);
join1([[_|_]=List|Rest], RelativeName, Result, OsType) ->
    join1(List++Rest, RelativeName, Result, OsType);
join1([[]|Rest], RelativeName, Result, OsType) ->
    join1(Rest, RelativeName, Result, OsType);
join1([Char|Rest], RelativeName, Result, OsType) when is_integer(Char) ->
    join1(Rest, RelativeName, [Char|Result], OsType);
join1([Atom|Rest], RelativeName, Result, OsType) when is_atom(Atom) ->
    join1(atom_to_list(Atom)++Rest, RelativeName, Result, OsType).

join1b(<<UcLetter, $:, Rest/binary>>, RelativeName, [], win32)
when is_integer(UcLetter), UcLetter >= $A, UcLetter =< $Z ->
    join1b(Rest, RelativeName, [$:, UcLetter+$a-$A], win32);
join1b(<<$\\,Rest/binary>>, RelativeName, Result, win32) ->
    join1b(<<$/,Rest/binary>>, RelativeName, Result, win32);
join1b(<<$/,Rest/binary>>, RelativeName, [$., $/|Result], OsType) ->
    join1b(Rest, RelativeName, [$/|Result], OsType);
join1b(<<$/,Rest/binary>>, RelativeName, [$/|Result], OsType) ->
    join1b(Rest, RelativeName, [$/|Result], OsType);
join1b(<<>>, <<>>, Result, OsType) ->
    list_to_binary(maybe_remove_dirsep(Result, OsType));
join1b(<<>>, RelativeName, [$:|Rest], win32) ->
    join1b(RelativeName, <<>>, [$:|Rest], win32);
join1b(<<>>, RelativeName, [$/|Result], OsType) ->
    join1b(RelativeName, <<>>, [$/|Result], OsType);
join1b(<<>>, RelativeName, [$., $/|Result], OsType) ->
    join1b(RelativeName, <<>>, [$/|Result], OsType);
join1b(<<>>, RelativeName, Result, OsType) ->
    join1b(RelativeName, <<>>, [$/|Result], OsType);
join1b(<<Char,Rest/binary>>, RelativeName, Result, OsType) when is_integer(Char) ->
    join1b(Rest, RelativeName, [Char|Result], OsType).

maybe_remove_dirsep([$/, $:, Letter], win32) ->
    [Letter, $:, $/];
maybe_remove_dirsep([$/], _) ->
    [$/];
maybe_remove_dirsep([$/|Name], _) ->
    lists:reverse(Name);
maybe_remove_dirsep(Name, _) ->
    lists:reverse(Name).

%% Appends a directory separator and a pathname component to
%% a given base directory, which is is assumed to be normalised
%% by a previous call to join/{1,2}.

-spec append(file:filename_all(), file:name_all()) -> file:filename_all().
append(Dir, Name) when is_binary(Dir), is_binary(Name) ->
    <<Dir/binary,$/:8,Name/binary>>;
append(Dir, Name) when is_binary(Dir) ->
    append(Dir,filename_string_to_binary(Name));
append(Dir, Name) when is_binary(Name) ->
    append(filename_string_to_binary(Dir),Name);
append(Dir, Name) ->
    Dir ++ [$/|Name].

%% Returns one of absolute, relative or volumerelative.
%%
%% absolute	The pathname refers to a specific file on a specific
%%		volume.  Example: /usr/local/bin/ (on Unix),
%%		h:/port_test (on Windows).
%% relative	The pathname is relative to the current working directory
%%		on the current volume.  Example:  foo/bar, ../src
%% volumerelative  The pathname is relative to the current working directory
%%		on the specified volume, or is a specific file on the
%%		current working volume.  (Windows only)
%%		Example: a:bar.erl, /temp/foo.erl

-spec pathtype(Path) -> 'absolute' | 'relative' | 'volumerelative' when
      Path :: file:name_all().
pathtype(Atom) when is_atom(Atom) ->
    pathtype(atom_to_list(Atom));
pathtype(Name) when is_list(Name) or is_binary(Name) ->
    case os:type() of
	{win32, _} ->
	    win32_pathtype(Name);
	{_, _}  ->
	    unix_pathtype(Name)
    end.

unix_pathtype(<<$/,_/binary>>) ->
    absolute;
unix_pathtype([$/|_]) ->
    absolute;
unix_pathtype([List|Rest]) when is_list(List) ->
    unix_pathtype(List++Rest);
unix_pathtype([Atom|Rest]) when is_atom(Atom) ->
    unix_pathtype(atom_to_list(Atom)++Rest);
unix_pathtype(_) ->
    relative.

win32_pathtype([List|Rest]) when is_list(List) ->
    win32_pathtype(List++Rest);
win32_pathtype([Atom|Rest]) when is_atom(Atom) ->
    win32_pathtype(atom_to_list(Atom)++Rest);
win32_pathtype([Char, List|Rest]) when is_list(List) ->
    win32_pathtype([Char|List++Rest]);
win32_pathtype(<<$/, $/, _/binary>>) -> absolute;
win32_pathtype(<<$\\, $/, _/binary>>) -> absolute;
win32_pathtype(<<$/, $\\, _/binary>>) -> absolute;
win32_pathtype(<<$\\, $\\, _/binary>>) -> absolute;
win32_pathtype(<<$/, _/binary>>) -> volumerelative;
win32_pathtype(<<$\\, _/binary>>) -> volumerelative;
win32_pathtype(<<_Letter, $:, $/, _/binary>>) -> absolute;
win32_pathtype(<<_Letter, $:, $\\, _/binary>>) -> absolute;
win32_pathtype(<<_Letter, $:, _/binary>>) -> volumerelative;
win32_pathtype([$/, $/|_]) -> absolute;
win32_pathtype([$\\, $/|_]) -> absolute;
win32_pathtype([$/, $\\|_]) -> absolute;
win32_pathtype([$\\, $\\|_]) -> absolute;
win32_pathtype([$/|_]) -> volumerelative;
win32_pathtype([$\\|_]) -> volumerelative;
win32_pathtype([C1, C2, List|Rest]) when is_list(List) ->
    pathtype([C1, C2|List++Rest]);
win32_pathtype([_Letter, $:, $/|_]) -> absolute;
win32_pathtype([_Letter, $:, $\\|_]) -> absolute;
win32_pathtype([_Letter, $:|_]) -> volumerelative;
win32_pathtype(_) 		  -> relative.

%% Returns all characters in the filename, except the extension.
%%
%% Examples: rootname("/jam.src/kalle") -> "/jam.src/kalle"
%%           rootname("/jam.src/foo.erl") -> "/jam.src/foo"

-spec rootname(Filename) -> file:filename_all() when
      Filename :: file:name_all().
rootname(Name) when is_binary(Name) ->
    list_to_binary(rootname(binary_to_list(Name))); % No need to handle unicode, . is < 128
rootname(Name0) ->
    Name = flatten(Name0),
    rootname(Name, [], [], major_os_type()).

rootname([$/|Rest], Root, Ext, OsType) ->
    rootname(Rest, [$/]++Ext++Root, [], OsType);
rootname([$\\|Rest], Root, Ext, win32) ->
    rootname(Rest, [$/]++Ext++Root, [], win32);
rootname([$.|Rest], Root, [], OsType) ->
    rootname(Rest, Root, ".", OsType);
rootname([$.|Rest], Root, Ext, OsType) ->
    rootname(Rest, Ext++Root, ".", OsType);
rootname([Char|Rest], Root, [], OsType) when is_integer(Char) ->
    rootname(Rest, [Char|Root], [], OsType);
rootname([Char|Rest], Root, Ext, OsType) when is_integer(Char) ->
    rootname(Rest, Root, [Char|Ext], OsType);
rootname([], Root, _Ext, _OsType) ->
    lists:reverse(Root).

%% Returns all characters in the filename, except the given extension.
%% If the filename has another extension, the complete filename is
%% returned.
%%
%% Examples: rootname("/jam.src/kalle.jam", ".erl") -> "/jam.src/kalle.jam"
%%           rootname("/jam.src/foo.erl", ".erl") -> "/jam.src/foo"

-spec rootname(Filename, Ext) -> file:filename_all() when
      Filename :: file:name_all(),
      Ext :: file:name_all().
rootname(Name, Ext) when is_binary(Name), is_binary(Ext) ->
    list_to_binary(rootname(binary_to_list(Name),binary_to_list(Ext)));
rootname(Name, Ext) when is_binary(Name) ->
    rootname(Name,filename_string_to_binary(Ext));
rootname(Name, Ext) when is_binary(Ext) ->
    rootname(filename_string_to_binary(Name),Ext);
rootname(Name0, Ext0) ->
    Name = flatten(Name0),
    Ext = flatten(Ext0),
    rootname2(Name, Ext, []).

rootname2(Ext, Ext, Result) ->
    lists:reverse(Result);
rootname2([], _Ext, Result) ->
    lists:reverse(Result);
rootname2([Char|Rest], Ext, Result) when is_integer(Char) ->
    rootname2(Rest, Ext, [Char|Result]).

%% Returns a list whose elements are the path components in the filename.
%%
%% Examples:	
%% split("/usr/local/bin") -> ["/", "usr", "local", "bin"]
%% split("foo/bar") -> ["foo", "bar"]
%% split("a:\\msdev\\include") -> ["a:/", "msdev", "include"]

-spec split(Filename) -> Components when
      Filename :: file:name_all(),
      Components :: [file:name_all()].
split(Name) when is_binary(Name) ->
    case os:type() of
	{win32, _} -> win32_splitb(Name);
	_  -> unix_splitb(Name)
    end;

split(Name0) ->
    Name = flatten(Name0),
    case os:type() of
	{win32, _} -> win32_split(Name);
	_  -> unix_split(Name)
    end.


unix_splitb(Name) ->
    L = binary:split(Name,[<<"/">>],[global]),
    LL = case L of
	     [<<>>|Rest] when Rest =/= [] ->
		 [<<"/">>|Rest];
	     _ ->
		 L
	 end,
    [ X || X <- LL, X =/= <<>>].


fix_driveletter(Letter0) ->
    if
	Letter0 >= $A, Letter0 =< $Z ->  
	    Letter0+$a-$A;
	true ->
	    Letter0
    end.
win32_splitb(<<Letter0,$:, Slash, Rest/binary>>) when (((Slash =:= $\\) orelse (Slash =:= $/)) andalso
							 ?IS_DRIVELETTER(Letter0)) ->
    Letter = fix_driveletter(Letter0),
    L = binary:split(Rest,[<<"/">>,<<"\\">>],[global]),
    [<<Letter,$:,$/>> | [ X || X <- L, X =/= <<>> ]]; 
win32_splitb(<<Letter0,$:,Rest/binary>>) when ?IS_DRIVELETTER(Letter0) ->
    Letter = fix_driveletter(Letter0),
    L = binary:split(Rest,[<<"/">>,<<"\\">>],[global]),
    [<<Letter,$:>> | [ X || X <- L, X =/= <<>> ]];
win32_splitb(<<Slash,Rest/binary>>) when ((Slash =:= $\\) orelse (Slash =:= $/)) ->
    L = binary:split(Rest,[<<"/">>,<<"\\">>],[global]),
    [<<$/>> | [ X || X <- L, X =/= <<>> ]];
win32_splitb(Name) ->
    L = binary:split(Name,[<<"/">>,<<"\\">>],[global]),
    [ X || X <- L, X =/= <<>> ].
    

unix_split(Name) ->
    split(Name, [], unix).

win32_split([$\\|Rest]) ->
    win32_split([$/|Rest]);
win32_split([X, $\\|Rest]) when is_integer(X) ->
    win32_split([X, $/|Rest]);
win32_split([X, Y, $\\|Rest]) when is_integer(X), is_integer(Y) ->
    win32_split([X, Y, $/|Rest]);
win32_split([UcLetter, $:|Rest]) when UcLetter >= $A, UcLetter =< $Z ->
    win32_split([UcLetter+$a-$A, $:|Rest]);
win32_split([Letter, $:, $/|Rest]) ->
    split(Rest, [], [[Letter, $:, $/]], win32);
win32_split([Letter, $:|Rest]) ->
    split(Rest, [], [[Letter, $:]], win32);
win32_split(Name) ->
    split(Name, [], win32).

split([$/|Rest], Components, OsType) ->
    split(Rest, [], [[$/]|Components], OsType);
split([$\\|Rest], Components, win32) ->
    split(Rest, [], [[$/]|Components], win32);
split(RelativeName, Components, OsType) ->
    split(RelativeName, [], Components, OsType).

split([$\\|Rest], Comp, Components, win32) ->
    split([$/|Rest], Comp, Components, win32);
split([$/|Rest], [], Components, OsType) ->
    split(Rest, [], Components, OsType);
split([$/|Rest], Comp, Components, OsType) ->
    split(Rest, [], [lists:reverse(Comp)|Components], OsType);
split([Char|Rest], Comp, Components, OsType) when is_integer(Char) ->
    split(Rest, [Char|Comp], Components, OsType);
split([], [], Components, _OsType) ->
    lists:reverse(Components);
split([], Comp, Components, OsType) ->
    split([], [], [lists:reverse(Comp)|Components], OsType).

%% Converts a filename to a form accepedt by the command shell and native
%% applications on the current platform.  On Windows, forward slashes
%% will be converted to backslashes.  On all platforms, the
%% name will be normalized as done by join/1.

-spec nativename(Path) -> file:filename_all() when
      Path :: file:name_all().
nativename(Name0) ->
    Name = join([Name0]),			%Normalize.
    case os:type() of
	{win32, _} -> win32_nativename(Name);
	_          -> Name
    end.

win32_nativename(Name) when is_binary(Name) ->
    binary:replace(Name, <<"/">>, <<"\\">>, [global]);
win32_nativename([$/|Rest]) ->
    [$\\|win32_nativename(Rest)];
win32_nativename([C|Rest]) ->
    [C|win32_nativename(Rest)];
win32_nativename([]) ->
    [].

separators() ->
    case os:type() of
	{win32, _} -> {$\\, $:};
	_ -> {false, false}
    end.

-spec safe_relative_path(Filename) -> 'unsafe' | SafeFilename when
      Filename :: file:name_all(),
      SafeFilename :: file:name_all().

safe_relative_path(Path) ->
    case pathtype(Path) of
        relative ->
            Cs0 = split(Path),
            safe_relative_path_1(Cs0, []);
        _ ->
            unsafe
    end.

safe_relative_path_1(["."|T], Acc) ->
    safe_relative_path_1(T, Acc);
safe_relative_path_1([<<".">>|T], Acc) ->
    safe_relative_path_1(T, Acc);
safe_relative_path_1([".."|T], Acc) ->
    climb(T, Acc);
safe_relative_path_1([<<"..">>|T], Acc) ->
    climb(T, Acc);
safe_relative_path_1([H|T], Acc) ->
    safe_relative_path_1(T, [H|Acc]);
safe_relative_path_1([], []) ->
    [];
safe_relative_path_1([], Acc) ->
    join(lists:reverse(Acc)).

climb(_, []) ->
    unsafe;
climb(T, [_|Acc]) ->
    safe_relative_path_1(T, Acc).



%% find_src(Module) --
%% find_src(Module, Rules) --
%%
%% Finds the source file name and compilation options for a compiled
%% module.  The result can be fed to compile:file/2 to compile the
%% file again.
%%
%% The Module argument (which can be a string or an atom) specifies 
%% either the module name or the path to the source code, with or 
%% without the ".erl" extension.  In either case the module must be
%% known by the code manager, i.e. code:which/1 should succeed.
%%
%% Rules describes how the source directory should be found given
%% the directory for the object code.  Each rule is on the form
%% {BinSuffix, SourceSuffix}, and is interpreted like this:
%% If the end of directory name where the object is located matches
%% BinSuffix, then the suffix will be replaced with SourceSuffix
%% in the directory name.  If the source file in the resulting
%% directory, the next rule will be tried.
%%
%% Returns: {SourceFile, Options}
%%
%% SourceFile is the absolute path to the source file (but without the ".erl"
%% extension) and Options are the necessary options to compile the file
%% with compile:file/2, but doesn't include options like 'report' or
%% 'verbose' that doesn't change the way code is generated.
%% The paths in the {outdir, Path} and {i, Path} options are guaranteed
%% to be absolute.

-spec find_src(Beam) -> {SourceFile, Options}
                      | {error, {ErrorReason, Module}} when
      Beam :: Module | Filename,
      Filename :: atom() | string(),
      Module :: module(),
      SourceFile :: string(),
      Options :: [Option],
      Option :: {'i', Path :: string()}
              | {'outdir', Path :: string()}
              | {'d', atom()},
      ErrorReason :: 'non_existing' | 'preloaded' | 'interpreted'.
find_src(Mod) ->
    Default = [{"", ""}, {"ebin", "src"}, {"ebin", "esrc"}],
    Rules = 
	case application:get_env(kernel, source_search_rules) of
	    undefined -> Default;
	    {ok, []} -> Default;
	    {ok, R} when is_list(R) -> R
	end,
    find_src(Mod, Rules).

-spec find_src(Beam, Rules) -> {SourceFile, Options}
                             | {error, {ErrorReason, Module}} when
      Beam :: Module | Filename,
      Filename :: atom() | string(),
      Rules :: [{BinSuffix :: string(), SourceSuffix :: string()}],
      Module :: module(),
      SourceFile :: string(),
      Options :: [Option],
      Option :: {'i', Path :: string()}
              | {'outdir', Path :: string()}
              | {'d', atom()},
      ErrorReason :: 'non_existing' | 'preloaded' | 'interpreted'.
find_src(Mod, Rules) when is_atom(Mod) ->
    find_src(atom_to_list(Mod), Rules);
find_src(File0, Rules) when is_list(File0) ->
    Mod = list_to_atom(basename(File0, ".erl")),
    File = rootname(File0, ".erl"),
    case readable_file(File++".erl") of
	true  ->
	    try_file(File, Mod, Rules);
	false ->
	    try_file(undefined, Mod, Rules)
    end.

try_file(File, Mod, Rules) ->
    case code:which(Mod) of
	Possibly_Rel_Path when is_list(Possibly_Rel_Path) ->
	    {ok, Cwd} = file:get_cwd(),
	    Path = join(Cwd, Possibly_Rel_Path),
	    try_file(File, Path, Mod, Rules);
	Ecode when is_atom(Ecode) -> % Ecode :: ecode()
	    {error, {Ecode, Mod}}
    end.

%% At this point, the Mod is known to be valid.
%% If the source name is not known, find it.
%% Then get the compilation options.
%% Returns: {SrcFile, Options}

try_file(undefined, ObjFilename, Mod, Rules) ->
    case get_source_file(ObjFilename, Mod, Rules) of
	{ok, File} -> try_file(File, ObjFilename, Mod, Rules);
	Error -> Error
    end;
try_file(Src, _ObjFilename, Mod, _Rules) ->
    List = case Mod:module_info(compile) of
	       none -> [];
	       List0 -> List0
	   end,
    Options = proplists:get_value(options, List, []),
    {ok, Cwd} = file:get_cwd(),
    AbsPath = make_abs_path(Cwd, Src),
    {AbsPath, filter_options(dirname(AbsPath), Options, [])}.

%% Filters the options.
%%
%% 1) Only keep options that have any effect on code generation.
%%
%% 2) The paths found in {i, Path} and {outdir, Path} are converted
%%    to absolute paths.  When doing this, it is assumed that relatives
%%    paths are relative to directory where the source code is located.
%%    This is not necessarily true.  It would be safer if the compiler
%%    would emit absolute paths in the first place.

filter_options(Base, [{outdir, Path}|Rest], Result) ->
    filter_options(Base, Rest, [{outdir, make_abs_path(Base, Path)}|Result]);
filter_options(Base, [{i, Path}|Rest], Result) ->
    filter_options(Base, Rest, [{i, make_abs_path(Base, Path)}|Result]);
filter_options(Base, [Option|Rest], Result) when Option =:= export_all ->
    filter_options(Base, Rest, [Option|Result]);
filter_options(Base, [Option|Rest], Result) when Option =:= binary ->
    filter_options(Base, Rest, [Option|Result]);
filter_options(Base, [Tuple|Rest], Result) when element(1, Tuple) =:= d ->
    filter_options(Base, Rest, [Tuple|Result]);
filter_options(Base, [Tuple|Rest], Result)
when element(1, Tuple) =:= parse_transform ->
    filter_options(Base, Rest, [Tuple|Result]);
filter_options(Base, [_|Rest], Result) ->
    filter_options(Base, Rest, Result);
filter_options(_Base, [], Result) ->
    Result.

%% Gets the source file given path of object code and module name.

get_source_file(Obj, Mod, Rules) ->
    source_by_rules(dirname(Obj), atom_to_list(Mod), Rules).

source_by_rules(Dir, Base, [{From, To}|Rest]) ->
    case try_rule(Dir, Base, From, To) of
	{ok, File} -> {ok, File};
	error      -> source_by_rules(Dir, Base, Rest)
    end;
source_by_rules(_Dir, _Base, []) ->
    {error, source_file_not_found}.

try_rule(Dir, Base, From, To) ->
    case lists:suffix(From, Dir) of
	true -> 
	    NewDir = lists:sublist(Dir, 1, length(Dir)-length(From))++To,
	    Src = join(NewDir, Base),
	    case readable_file(Src++".erl") of
		true -> {ok, Src};
		false -> error
	    end;
	false ->
	    error
    end.

readable_file(File) ->
    case file:read_file_info(File) of
	{ok, #file_info{type=regular, access=read}} ->
	    true;
	{ok, #file_info{type=regular, access=read_write}} ->
	    true;
	_Other ->
	    false
    end.

make_abs_path(BasePath, Path) ->
    join(BasePath, Path).

major_os_type() ->
    {OsT, _} = os:type(),
    OsT.

%% flatten(List)
%%  Flatten a list, also accepting atoms.

-spec flatten(Filename) -> file:filename_all() when
      Filename :: file:name_all().
flatten(Bin) when is_binary(Bin) ->
    Bin;
flatten(List) ->
    do_flatten(List, []).

do_flatten([H|T], Tail) when is_list(H) ->
    do_flatten(H, do_flatten(T, Tail));
do_flatten([H|T], Tail) when is_atom(H) ->
    atom_to_list(H) ++ do_flatten(T, Tail);
do_flatten([H|T], Tail) ->
    [H|do_flatten(T, Tail)];
do_flatten([], Tail) ->
    Tail;
do_flatten(Atom, Tail) when is_atom(Atom) ->
    atom_to_list(Atom) ++ flatten(Tail).

filename_string_to_binary(List) ->
    case unicode:characters_to_binary(flatten(List),unicode,file:native_name_encoding()) of
	{error,_,_} ->
	    erlang:error(badarg);
	Bin when is_binary(Bin) ->
	    Bin
    end.

%% Application Base Directories
%% basedir
%% http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html

-type basedir_type() :: 'user_cache' | 'user_config' | 'user_data'
                      | 'user_log'
                      | 'site_config' | 'site_data'.

-spec basedir(Type,Application) -> file:filename_all() when
      Type :: basedir_type(),
      Application :: string() | binary().

basedir(Type,Application) when is_atom(Type), is_list(Application) orelse
                                              is_binary(Application) ->
    basedir(Type, Application, #{}).

-spec basedir(Type,Application,Opts) -> file:filename_all() when
      Type :: basedir_type(),
      Application :: string() | binary(),
      Opts :: #{author => string() | binary(),
                os => 'windows' | 'darwin' | 'linux',
                version => string() | binary()}.

basedir(Type,Application,Opts) when is_atom(Type), is_map(Opts),
                                    is_list(Application) orelse
                                    is_binary(Application) ->
    Os   = basedir_os_from_opts(Opts),
    Name = basedir_name_from_opts(Os,Application,Opts),
    Base = basedir_from_os(Type,Os),
    case {Type,Os} of
        {user_log,linux} ->
            filename:join([Base,Name,"log"]);
        {user_log,windows} ->
            filename:join([Base,Name,"Logs"]);
        {user_cache,windows} ->
            filename:join([Base,Name,"Cache"]);
        {Type,_} when Type =:= site_config orelse Type =:= site_data ->
            [filename:join([B,Name]) || B <- Base];
        _ ->
            filename:join([Base,Name])
    end.

basedir_os_from_opts(#{os := linux}) -> linux;
basedir_os_from_opts(#{os := windows}) -> windows;
basedir_os_from_opts(#{os := darwin}) -> darwin;
basedir_os_from_opts(#{}) -> basedir_os_type().

basedir_name_from_opts(windows,App,#{author:=Author,version:=Vsn}) ->
    filename:join([Author,App,Vsn]);
basedir_name_from_opts(windows,App,#{author:=Author}) ->
    filename:join([Author,App]);
basedir_name_from_opts(_,App,#{version:=Vsn}) ->
    filename:join([App,Vsn]);
basedir_name_from_opts(_,App,_) ->
    App.

basedir_from_os(Type,Os) ->
    case Os of
        linux   -> basedir_linux(Type);
        darwin  -> basedir_darwin(Type);
        windows -> basedir_windows(Type)
    end.

-define(basedir_linux_user_data,   ".local/share").
-define(basedir_linux_user_config, ".config").
-define(basedir_linux_user_cache,  ".cache").
-define(basedir_linux_user_log,    ".cache"). %% .cache/App/log
-define(basedir_linux_site_data,   "/usr/local/share/:/usr/share/").
-define(basedir_linux_site_config, "/etc/xdg").

basedir_linux(Type) ->
    case Type of
        user_data   -> getenv("XDG_DATA_HOME",  ?basedir_linux_user_data,  true);
        user_config -> getenv("XDG_CONFIG_HOME",?basedir_linux_user_config,true);
        user_cache  -> getenv("XDG_CACHE_HOME", ?basedir_linux_user_cache, true);
        user_log    -> getenv("XDG_CACHE_HOME", ?basedir_linux_user_log,   true);
        site_data   ->
            Base = getenv("XDG_DATA_DIRS",?basedir_linux_site_data,false),
            string:tokens(Base,":");
        site_config ->
            Base = getenv("XDG_CONFIG_DIRS",?basedir_linux_site_config,false),
            string:tokens(Base,":")
    end.

-define(basedir_darwin_user_data,   "Library/Application Support").
-define(basedir_darwin_user_config, "Library/Application Support").
-define(basedir_darwin_user_cache,  "Library/Caches").
-define(basedir_darwin_user_log,    "Library/Logs").
-define(basedir_darwin_site_data,   "/Library/Application Support").
-define(basedir_darwin_site_config, "/Library/Application Support").

basedir_darwin(Type) ->
    case Type of
        user_data   -> basedir_join_home(?basedir_darwin_user_data);
        user_config -> basedir_join_home(?basedir_darwin_user_config);
        user_cache  -> basedir_join_home(?basedir_darwin_user_cache);
        user_log    -> basedir_join_home(?basedir_darwin_user_log);
        site_data   -> [?basedir_darwin_site_data];
        site_config -> [?basedir_darwin_site_config]
    end.

%% On Windows:
%% ex. C:\Users\egil\AppData\Local\Ericsson\Erlang
%% %LOCALAPPDATA% is defined on Windows 7 and onwards
%% %APPDATA% is used instead of %LOCALAPPDATA% if it's not defined.
%% %APPDATA% is used for roaming, i.e. for user_config on Windows 7 and beyond.
%%
%% user_data    %LOCALAPPDATA%[/$author]/$appname[/$version]
%% user_config  %APPDATA%[/$author]/$appname[/$version]
%% user_cache   %LOCALAPPDATA%[/$author]/$appname[/$version]/Cache
%% user_log     %LOCALAPPDATA%[/$author]/$appname[/$version]/Logs

-define(basedir_windows_user_data,   "Local").
-define(basedir_windows_user_config, "Roaming").
-define(basedir_windows_user_cache,  "Local").    %% Cache is added later
-define(basedir_windows_user_log,    "Local").    %% Logs is added later

basedir_windows(Type) ->
    %% If LOCALAPPDATA is not defined we are likely on an
    %% XP machine. Use APPDATA instead.
    case basedir_windows_appdata() of
        noappdata ->
            %% No AppData is set
            %% Probably running MSYS
            case Type of
                user_data   -> basedir_join_home(?basedir_windows_user_data);
                user_config -> basedir_join_home(?basedir_windows_user_config);
                user_cache  -> basedir_join_home(?basedir_windows_user_cache);
                user_log    -> basedir_join_home(?basedir_windows_user_log);
                site_data   -> [];
                site_config -> []
            end;
        {ok, AppData} ->
            case Type of
                user_data   -> getenv("LOCALAPPDATA", AppData);
                user_config -> AppData;
                user_cache  -> getenv("LOCALAPPDATA", AppData);
                user_log    -> getenv("LOCALAPPDATA", AppData);
                site_data   -> [];
                site_config -> []
            end
    end.

basedir_windows_appdata() ->
    case os:getenv("APPDATA") of
        Invalid when Invalid =:= false orelse Invalid =:= [] ->
            noappdata;
        Val ->
            {ok, Val}
    end.

%% basedir aux

getenv(K,Def,false) -> getenv(K,Def);
getenv(K,Def,true)  -> getenv(K,basedir_join_home(Def)).

getenv(K,Def) ->
    case os:getenv(K) of
        []    -> Def;
        false -> Def;
        Val   -> Val
    end.

basedir_join_home(Dir) ->
    case os:getenv("HOME") of
        false ->
            {ok,[[Home]]} = init:get_argument(home),
            filename:join(Home,Dir);
        Home  -> filename:join(Home,Dir)
    end.

basedir_os_type() ->
    case os:type() of
        {unix,darwin} -> darwin;
        {win32,_}     -> windows;
        _             -> linux
    end.
