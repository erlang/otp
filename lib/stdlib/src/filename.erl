%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
	 rootname/1, rootname/2, split/1, nativename/1]).
-export([find_src/1, find_src/2, flatten/1]).

%% Undocumented and unsupported exports.
-export([append/2]).

-include_lib("kernel/include/file.hrl").

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

-spec absname(file:name()) -> string().
absname(Name) ->
    {ok, Cwd} = file:get_cwd(),
    absname(Name, Cwd).

-spec absname(file:name(), string()) -> string().
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

%% Joins a relative filename to an absolute base. For VxWorks the 
%% resulting name is  fixed to minimize the length by collapsing 
%% ".." directories.
%% For other systems this is just a join/2, but assumes that 
%% AbsBase must be absolute and Name must be relative.

-spec absname_join(string(), file:name()) -> string().
absname_join(AbsBase, Name) ->
    case major_os_type() of
	vxworks -> 
	    absname_pretty(AbsBase, split(Name), lists:reverse(split(AbsBase)));
	_Else ->
	    join(AbsBase, flatten(Name))
    end.

%% Handles absolute filenames for VxWorks - these are 'pretty-printed',
%% since a C function call chdir("/erlang/lib/../bin") really sets
%% cwd to '/erlang/lib/../bin' which also works, but the long term
%% effect is potentially not so good ...
%%
%% absname_pretty("../bin", "/erlang/lib") -> "/erlang/bin"
%% absname_pretty("../../../..", "/erlang") -> "/erlang"

absname_pretty(Abspath, Relpath, []) ->
    %% AbsBase _must_ begin with a vxworks device name
    {device, _Rest, Dev} = vxworks_first(Abspath),
    absname_pretty(Abspath, Relpath, [lists:reverse(Dev)]);
absname_pretty(_Abspath, [], AbsBase) ->
    join(lists:reverse(AbsBase));
absname_pretty(Abspath, [[$.]|Rest], AbsBase) ->
    absname_pretty(Abspath, Rest, AbsBase);
absname_pretty(Abspath, [[$.,$.]|Rest], [_|AbsRest]) ->
    absname_pretty(Abspath, Rest, AbsRest);
absname_pretty(Abspath, [First|Rest], AbsBase) ->
    absname_pretty(Abspath, Rest, [First|AbsBase]).

%% Returns the part of the filename after the last directory separator,
%% or the filename itself if it has no separators.
%%
%% Examples: basename("foo") -> "foo"
%%           basename("/usr/foo") -> "foo"
%%           basename("/usr/foo/") -> "foo"  (trailing slashes ignored)
%%           basename("/") -> []

-spec basename(file:name()) -> string().
basename(Name0) ->
    Name = flatten(Name0),
    {DirSep2, DrvSep} = separators(),
    basename1(skip_prefix(Name, DrvSep), [], DirSep2).

basename1([$/|[]], Tail, DirSep2) ->
    basename1([], Tail, DirSep2);
basename1([$/|Rest], _Tail, DirSep2) ->
    basename1(Rest, [], DirSep2);
basename1([[_|_]=List|Rest], Tail, DirSep2) ->
    basename1(List++Rest, Tail, DirSep2);
basename1([DirSep2|Rest], Tail, DirSep2) when is_integer(DirSep2) ->
    basename1([$/|Rest], Tail, DirSep2);
basename1([Char|Rest], Tail, DirSep2) when is_integer(Char) ->
    basename1(Rest, [Char|Tail], DirSep2);
basename1([], Tail, _DirSep2) ->
    lists:reverse(Tail).

skip_prefix(Name, false) ->	% No prefix for unix, but for VxWorks.
    case major_os_type() of
	vxworks -> 
	    case vxworks_first(Name) of
		{device, Rest, _Device} ->
		    Rest;
		{not_device, _Rest, _First} ->
		    Name
	    end;
	_Else ->
	    Name
    end;
skip_prefix(Name, DrvSep) ->
    skip_prefix1(Name, DrvSep).

skip_prefix1([L, DrvSep|Name], DrvSep) when is_integer(L) ->
    Name;
skip_prefix1([L], _) when is_integer(L) ->
    [L];
skip_prefix1(Name, _) ->
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

-spec basename(file:name(), file:name()) -> string().
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
basename([$\\|Rest], Ext, Tail, DirSep2) when is_integer(DirSep2) ->
    basename([$/|Rest], Ext, Tail, DirSep2);
basename([Char|Rest], Ext, Tail, DrvSep2) when is_integer(Char) ->
    basename(Rest, Ext, [Char|Tail], DrvSep2);
basename([], _Ext, Tail, _DrvSep2) ->
    lists:reverse(Tail).

%% Returns the directory part of a pathname.
%%
%% Example: dirname("/usr/src/kalle.erl") -> "/usr/src",
%%	    dirname("kalle.erl") -> "."

-spec dirname(file:name()) -> string().
dirname(Name0) ->
    Name = flatten(Name0),
    case os:type() of
	vxworks ->
	    {Devicep, Restname, FirstComp} = vxworks_first(Name),
	    case Devicep of
		device ->
		    dirname(Restname, FirstComp, [], separators());
		_ -> 
		    dirname(Name, [], [], separators())
	    end;
	_ ->
	    dirname(Name, [], [], separators())
    end.

dirname([[_|_]=List|Rest], Dir, File, Seps) ->
    dirname(List++Rest, Dir, File, Seps);
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
    
%% Given a filename string, returns the file extension,
%% including the period.  Returns an empty list if there
%% is no extension.
%%
%% Example: extension("foo.erl") -> ".erl"
%%	    extension("jam.src/kalle") -> ""
%%
%% On Windows:  fn:dirname("\\usr\\src/kalle.erl") -> "/usr/src"

-spec extension(file:name()) -> string().
extension(Name0) ->
    Name = flatten(Name0),
    extension(Name, [], major_os_type()).

extension([$.|Rest], _Result, OsType) ->
    extension(Rest, [$.], OsType);
extension([Char|Rest], [], OsType) when is_integer(Char) ->
    extension(Rest, [], OsType);
extension([$/|Rest], _Result, OsType) ->
    extension(Rest, [], OsType);
extension([$\\|Rest], _Result, win32) ->
    extension(Rest, [], win32);
extension([$\\|Rest], _Result, vxworks) ->
    extension(Rest, [], vxworks);
extension([Char|Rest], Result, OsType) when is_integer(Char) ->
    extension(Rest, [Char|Result], OsType);
extension([], Result, _OsType) ->
    lists:reverse(Result).

%% Joins a list of filenames with directory separators.

-spec join([string()]) -> string().
join([Name1, Name2|Rest]) ->
    join([join(Name1, Name2)|Rest]);
join([Name]) when is_list(Name) ->
    join1(Name, [], [], major_os_type());
join([Name]) when is_atom(Name) ->
    join([atom_to_list(Name)]).

%% Joins two filenames with directory separators.

-spec join(string(), string()) -> string().
join(Name1, Name2) when is_list(Name1), is_list(Name2) ->
    OsType = major_os_type(),
    case pathtype(Name2) of
	relative -> join1(Name1, Name2, [], OsType);
	_Other -> join1(Name2, [], [], OsType)
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
join1([$\\|Rest], RelativeName, Result, vxworks) ->
    join1([$/|Rest], RelativeName, Result, vxworks);
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

-spec append(string(), file:name()) -> string().
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

-spec pathtype(file:name()) -> 'absolute' | 'relative' | 'volumerelative'.
pathtype(Atom) when is_atom(Atom) ->
    pathtype(atom_to_list(Atom));
pathtype(Name) when is_list(Name) ->
    case os:type() of
	{unix, _}  -> unix_pathtype(Name);
	{win32, _} -> win32_pathtype(Name);
	vxworks ->  case vxworks_first(Name) of
			{device, _Rest, _Dev} ->
			    absolute;
			_ ->
			    relative
		    end;
	{ose,_} -> unix_pathtype(Name)
    end.

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

-spec rootname(file:name()) -> string().
rootname(Name0) ->
    Name = flatten(Name0),
    rootname(Name, [], [], major_os_type()).

rootname([$/|Rest], Root, Ext, OsType) ->
    rootname(Rest, [$/]++Ext++Root, [], OsType);
rootname([$\\|Rest], Root, Ext, win32) ->
    rootname(Rest, [$/]++Ext++Root, [], win32);
rootname([$\\|Rest], Root, Ext, vxworks) ->
    rootname(Rest, [$/]++Ext++Root, [], vxworks);
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

-spec rootname(file:name(), file:name()) -> string().
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

-spec split(file:name()) -> [string()].
split(Name0) ->
    Name = flatten(Name0),
    case os:type() of
	{unix, _}  -> unix_split(Name);
	{win32, _} -> win32_split(Name);
	vxworks -> vxworks_split(Name);
	{ose,_} -> unix_split(Name)
    end.

%% If a VxWorks filename starts with '[/\].*[^/\]' '[/\].*:' or '.*:' 
%% that part of the filename is considered a device.  
%% The rest of the name is interpreted exactly as for win32.

%% XXX - dirty solution to make filename:split([]) return the same thing on
%%       VxWorks as on unix and win32.
vxworks_split([]) ->
    [];
vxworks_split(L) -> 
    {_Devicep, Rest, FirstComp} = vxworks_first(L),
    split(Rest, [], [lists:reverse(FirstComp)], win32).

unix_split(Name) ->
    split(Name, [], unix).

win32_split([$\\|Rest]) ->
    win32_split([$/|Rest]);
win32_split([X, $\\|Rest]) when is_integer(X) ->
    win32_split([X, $/|Rest]);
win32_split([X, Y, $\\|Rest]) when is_integer(X), is_integer(Y) ->
    win32_split([X, Y, $/|Rest]);
win32_split([$/, $/|Rest]) ->
    split(Rest, [], [[$/, $/]]);
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
split([List|Rest], Comp, Components, OsType) when is_list(List) ->
    split(List++Rest, Comp, Components, OsType);
split([], [], Components, _OsType) ->
    lists:reverse(Components);
split([], Comp, Components, OsType) ->
    split([], [], [lists:reverse(Comp)|Components], OsType).

%% Converts a filename to a form accepedt by the command shell and native
%% applications on the current platform.  On Windows, forward slashes
%% will be converted to backslashes.  On all platforms, the
%% name will be normalized as done by join/1.

-spec nativename(string()) -> string().
nativename(Name0) ->
    Name = join([Name0]),			%Normalize.
    case os:type() of
	{win32, _} -> win32_nativename(Name);
	_          -> Name
    end.

win32_nativename([$/|Rest]) ->
    [$\\|win32_nativename(Rest)];
win32_nativename([C|Rest]) ->
    [C|win32_nativename(Rest)];
win32_nativename([]) ->
    [].

separators() ->
    case os:type() of
	{unix, _}  -> {false, false};
	{win32, _} -> {$\\, $:};
	vxworks -> {$\\, false};
	{ose,_} -> {false, false}
    end.


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

-type rule()   :: {string(), string()}.
-type ecode()  :: 'non_existing' | 'preloaded' | 'interpreted'.
-type option() :: {'i', string()} | {'outdir', string()} | {'d', atom()}.

-spec find_src(atom() | string()) ->
	     {string(), [option()]} | {'error', {ecode(), atom()}}.
find_src(Mod) ->
    Default = [{"", ""}, {"ebin", "src"}, {"ebin", "esrc"}],
    Rules = 
	case application:get_env(kernel, source_search_rules) of
	    undefined -> Default;
	    {ok, []} -> Default;
	    {ok, R} when is_list(R) -> R
	end,
    find_src(Mod, Rules).

-spec find_src(atom() | string(), [rule()]) ->
	     {string(), [option()]} | {'error', {ecode(), atom()}}.
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
    List = Mod:module_info(compile),
    {options, Options} = lists:keyfind(options, 1, List),
    {ok, Cwd} = file:get_cwd(),
    AbsPath = make_abs_path(Cwd, Src),
    {AbsPath, filter_options(dirname(AbsPath), Options, [])}.

%% Filters the options.
%%
%% 1) Remove options that have no effect on the generated code,
%%    such as report and verbose.
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
filter_options(Base, [Option|Rest], Result) when Option =:= trace ->
    filter_options(Base, Rest, [Option|Result]);
filter_options(Base, [Option|Rest], Result) when Option =:= export_all ->
    filter_options(Base, Rest, [Option|Result]);
filter_options(Base, [Option|Rest], Result) when Option =:= binary ->
    filter_options(Base, Rest, [Option|Result]);
filter_options(Base, [Option|Rest], Result) when Option =:= fast ->
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
    case catch Mod:module_info(source_file) of
	{'EXIT', _Reason} ->
	    source_by_rules(dirname(Obj), packages:last(Mod), Rules);
	File ->
	    {ok, File}
    end.

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
    case os:type() of
	{OsT, _} -> OsT;
	OsT -> OsT
    end.

%% Need to take care of the first pathname component separately
%% due to VxWorks less than good device naming rules.
%% (i.e. this is VxWorks specific ...)
%% The following four all starts with device names
%% elrond:/foo -> elrond:
%% elrond:\\foo.bar -> elrond:
%% /DISK1:foo -> /DISK1:
%% /usr/include -> /usr
%% This one doesn't:
%% foo/bar

vxworks_first([]) ->
    {not_device, [], []};
vxworks_first([$/|T]) ->
    vxworks_first2(device, T, [$/]);
vxworks_first([$\\|T]) ->
    vxworks_first2(device, T, [$/]);
vxworks_first([H|T]) when is_list(H) ->
    vxworks_first(H++T);
vxworks_first([H|T]) ->
    vxworks_first2(not_device, T, [H]).

vxworks_first2(Devicep, [], FirstComp) ->
    {Devicep, [], FirstComp};
vxworks_first2(Devicep, [$/|T], FirstComp) ->
    {Devicep, [$/|T], FirstComp};
vxworks_first2(Devicep, [$\\|T], FirstComp) ->
    {Devicep, [$/|T], FirstComp};
vxworks_first2(_Devicep, [$:|T], FirstComp)->
    {device, T, [$:|FirstComp]};
vxworks_first2(Devicep, [H|T], FirstComp) when is_list(H) ->
    vxworks_first2(Devicep, H++T, FirstComp);
vxworks_first2(Devicep, [H|T], FirstComp) ->
    vxworks_first2(Devicep, T, [H|FirstComp]).
    
%% flatten(List)
%%  Flatten a list, also accepting atoms.

-spec flatten(file:name()) -> string().
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
