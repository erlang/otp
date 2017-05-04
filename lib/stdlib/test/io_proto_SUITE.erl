%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2017. All Rights Reserved.
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
-module(io_proto_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([setopts_getopts/1,unicode_options/1,unicode_options_gen/1, 
	 binary_options/1, read_modes_gl/1,
	 read_modes_ogl/1, broken_unicode/1,eof_on_pipe/1,unicode_prompt/1]).


-export([io_server_proxy/1,start_io_server_proxy/0, proxy_getall/1, 
	 proxy_setnext/2, proxy_quit/1]).
%% For spawn
-export([toerl_server/3,answering_machine1/3,
	 answering_machine2/3]).

-export([uprompt/1]).

%%-define(without_test_server, true).

-ifdef(without_test_server).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), foo).
-define(t, test_server).
-define(privdir(_), "./io_SUITE_priv").
-else.
-include_lib("common_test/include/ct.hrl").
-define(privdir(Conf), proplists:get_value(priv_dir, Conf)).
-endif.

%%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(dbg(Data),io:format(standard_error, "DBG: ~p\r\n",[Data])).
-define(RM_RF(Dir),begin io:format(standard_error, "Not Removed: ~p\r\n",[Dir]), 
			 ok end).
-else.
-define(format(S, A), ok).
-define(dbg(Data),noop).
-define(RM_RF(Dir),rm_rf(Dir)).
-endif.

init_per_testcase(_Case, Config) ->
    Term = os:getenv("TERM", "dumb"),
    os:putenv("TERM","vt100"),
    [{term, Term} | Config].
end_per_testcase(_Case, Config) ->
    Term = proplists:get_value(term,Config),
    os:putenv("TERM",Term),
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,5}}].

all() -> 
    [setopts_getopts, unicode_options, unicode_options_gen,
     binary_options, read_modes_gl, read_modes_ogl,
     broken_unicode, eof_on_pipe, unicode_prompt].

groups() -> 
    [].

init_per_suite(Config) ->
    DefShell = get_default_shell(),
    [{default_shell,DefShell}|Config].

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



-record(state, {
	  q = [],
	  nxt = eof,
	  mode = list
	 }).

uprompt(_L) ->
    [1050,1072,1082,1074,1086,32,1077,32,85,110,105,99,111,100,101,32,63].

%% Test that an Unicode prompt does not crash the shell.
unicode_prompt(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    case proplists:get_value(default_shell,Config) of
	old ->
	    ok;
	new ->
	    rtnode([{putline,""},
		    {putline, "2."},
		    {getline, "2"},
		    {putline, "shell:prompt_func({io_proto_SUITE,uprompt})."},
		    {getline, "default"},
		    {putline, "io:get_line('')."},
		    {putline, "hej"},
		    {getline, "\"hej\\n\""},
		    {putline, "io:setopts([{binary,true}])."},
		    {getline, "ok"},
		    {putline, "io:get_line('')."},
		    {putline, "hej"},
		    {getline, "<<\"hej\\n\">>"}
		   ],[],[],"-pa \""++ PA++"\"")
    end,
    %% And one with oldshell
    rtnode([{putline,""},
	    {putline, "2."},
	    {getline_re, ".*2$"},
	    {putline, "shell:prompt_func({io_proto_SUITE,uprompt})."},
	    {getline_re, ".*default"},
	    {putline, "io:get_line('')."},
	    {putline, "hej"},
	    {getline_re, ".*\"hej\\\\n\""},
	    {putline, "io:setopts([{binary,true}])."},
	    {getline_re, ".*ok"},
	    {putline, "io:get_line('')."},
	    {putline, "hej"},
	    {getline_re, ".*<<\"hej\\\\n\">>"}
	   ],[],[],"-oldshell -pa \""++PA++"\""),
    ok.


%% Check io:setopts and io:getopts functions.
setopts_getopts(Config) when is_list(Config) ->
    FileName = filename:join([proplists:get_value(priv_dir,Config),
			      "io_proto_SUITE_setopts_getopts.dat"]),
    {ok,WFile} = file:open(FileName,[write]),
    Server = start_io_server_proxy(),
    [{binary, false}] = io:getopts(Server),
    [getopts] = proxy_getall(Server),
    [{binary,false},{encoding,latin1}] = lists:sort(io:getopts(WFile)),
    proxy_setnext(Server,"Hej"),
    "Hej" = io:get_line(Server,''),
    proxy_setnext(Server,"Hej"++[532]),
    [$H,$e,$j,532] = io:get_line(Server,''),
    ok = io:setopts(Server,[{binary,true}]),
    proxy_setnext(Server,"Hej"),
    <<"Hej">> = io:get_line(Server,''),
    proxy_setnext(Server,"Hej"++[532]),
    <<72,101,106,200,148>> = io:get_line(Server,''),
    [$H,$e,$j,532] = lists:flatten(io_lib:format("~ts",[<<72,101,106,200,148>>])),
    file:write(WFile,<<"HejA">>),
    file:write(WFile,unicode:characters_to_binary("Hej"++[532],unicode,unicode)),
    file:write(WFile,unicode:characters_to_binary("Hej"++[532],unicode,{utf16,big})),
    file:write(WFile,unicode:characters_to_binary("Hej"++[532],unicode,{utf16,little})),
    file:write(WFile,unicode:characters_to_binary("Hej"++[532],unicode,{utf32,big})),
    file:write(WFile,unicode:characters_to_binary("Hej"++[532],unicode,{utf32,little})),
    file:close(WFile),
    {ok,RFile} = file:open(FileName,[read]),
    [{binary,false},{encoding,latin1}] = lists:sort(io:getopts(RFile)),
    [$H,$e,$j,$A] = io:get_chars(RFile,'',4),
    io:setopts(RFile,[{encoding,unicode}]),
    [$H,$e,$j,532] = io:get_chars(RFile,'',4),
    [{binary,false},{encoding,unicode}] = lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf16,big}}]),
    [$H,$e,$j,532] = io:get_chars(RFile,'',4),
    [{binary,false},{encoding,{utf16,big}}] =
	lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf16,little}}]),
    [$H,$e,$j,532] = io:get_chars(RFile,'',4),
    [{binary,false},{encoding,{utf16,little}}] =
	lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf32,big}}]),
    [$H,$e,$j,532] = io:get_chars(RFile,'',4),
    [{binary,false},{encoding,{utf32,big}}] =
	lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf32,little}}]),
    [$H,$e,$j,532] = io:get_chars(RFile,'',4),
    [{binary,false},{encoding,{utf32,little}}] =
	lists:sort(io:getopts(RFile)),
    eof = io:get_line(RFile,''),
    file:position(RFile,0),
    io:setopts(RFile,[{binary,true},{encoding,latin1}]),
    <<$H,$e,$j,$A>> = io:get_chars(RFile,'',4),
    [{binary,true},{encoding,latin1}] = lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,unicode}]),
    <<$H,$e,$j,532/utf8>> = io:get_chars(RFile,'',4),
    [{binary,true},{encoding,unicode}] = lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf16,big}}]),
    <<$H,$e,$j,532/utf8>> = io:get_chars(RFile,'',4),
    [{binary,true},{encoding,{utf16,big}}] =
	lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf16,little}}]),
    <<$H,$e,$j,532/utf8>> = io:get_chars(RFile,'',4),
    [{binary,true},{encoding,{utf16,little}}] =
	lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf32,big}}]),
    <<$H,$e,$j,532/utf8>> = io:get_chars(RFile,'',4),
    [{binary,true},{encoding,{utf32,big}}] =
	lists:sort(io:getopts(RFile)),
    io:setopts(RFile,[{encoding,{utf32,little}}]),
    <<$H,$e,$j,532/utf8>> = io:get_chars(RFile,'',4),
    [{binary,true},{encoding,{utf32,little}}] =
	lists:sort(io:getopts(RFile)),
    eof = io:get_line(RFile,''),
    file:close(RFile),
    case proplists:get_value(default_shell,Config) of
	old ->
	    ok;
	new ->
	    %% So, lets test another node with new interactive shell
	    rtnode([{putline,""},
		    {putline, "2."},
		    {getline, "2"},
		    {putline, "lists:keyfind(binary,1,io:getopts())."},
		    {getline, "{binary,false}"},
		    {putline, "io:get_line('')."},
		    {putline, "hej"},
		    {getline, "\"hej\\n\""},
		    {putline, "io:setopts([{binary,true}])."},
		    {getline, "ok"},
		    {putline, "io:get_line('')."},
		    {putline, "hej"},
		    {getline, "<<\"hej\\n\">>"}
		   ],[])
    end,
    %% And one with oldshell
    rtnode([{putline,""},
	    {putline, "2."},
	    {getline_re, ".*2$"},
	    {putline, "lists:keyfind(binary,1,io:getopts())."},
	    {getline_re, ".*{binary,false}"},
	    {putline, "io:get_line('')."},
	    {putline, "hej"},
	    {getline_re, ".*\"hej\\\\n\""},
	    {putline, "io:setopts([{binary,true}])."},
	    {getline_re, ".*ok"},
	    {putline, "io:get_line('')."},
	    {putline, "hej"},
	    {getline_re, ".*<<\"hej\\\\n\">>"}
	   ],[],[],"-oldshell"),
    ok.


get_lc_ctype() ->
    case {os:type(),os:version()} of
	{{unix,sunos},{5,N,_}} when N =< 8 ->
	    "iso_8859_1";
	_ ->
	    "ISO-8859-1"
    end.

%% Test various unicode options.
unicode_options(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    PrivDir = proplists:get_value(priv_dir,Config),
    %% A string in both russian and greek characters, which is present
    %% in all the internal test files (but in different formats of course)...
    TestData = [1090,1093,1077,32,1073,1080,1075,32,
		1088,1077,1076,32,1092,1086,1100,32,1093,
		1072,1089,32,1089,1086,1100,32,932,951,949,
		32,946,953,947,32,961,949,948,32,
		963,959,967,32,945,961,949,32,966,959,967,949,963],
    %% Testdata from Chinese open source customer, that triggered OTP-7974
    TestData2 = [46,46,46,12411,12370,12411,12370,44,12411,12370,12411,12370,44,
		 12411,12370,12411,12370,44,12411,12370,12411,12370,44,12411,12370,
		 12411,12370,44,44,44,12411,12370,12411,12370,44,44,12411,12370,12411,
		 12370,44,12411,12370,12411,12370,44,12411,12370,12411,12370,44,12411,
		 12370,12411,12370,44,12411,12370,12411,12370,44,44,44,10],

    %% The external test files are generated with a BOM writing
    %% text editor. A shorter line is written (with two characters 
    %% larger than 127).
    ExternalTestData = [197,116,101,114,101,114,246,118,114,97],
    InternalBomFiles = ["testdata_utf8_bom.dat",
			"testdata_utf16_big_bom.dat",
			"testdata_utf16_little_bom.dat",
			"testdata_utf32_big_bom.dat",
			"testdata_utf32_little_bom.dat"],
    AllNoBom = [{utf8,"testdata_utf8.dat"},
		{utf16,"testdata_utf16_big.dat"},
		{{utf16,big},"testdata_utf16_big.dat"},
		{{utf16,little},"testdata_utf16_little.dat"},
		{utf32,"testdata_utf32_big.dat"},
		{{utf32,big},"testdata_utf32_big.dat"},
		{{utf32,little},"testdata_utf32_little.dat"}],
    ExternalBomFiles = ["external_utf8_bom.dat",
			"external_utf16_little_bom.dat",
			"external_utf16_big_bom.dat"],
    ReadBomFile = fun(File,Dir) ->
			  {ok,F} = file:open(filename:join([Dir,File]),
					     [read,binary]),
			  {ok,Bin} = file:read(F,4),
			  {Type,Bytes} = unicode:bom_to_encoding(Bin),
			  file:position(F,Bytes),
			  io:setopts(F,[{encoding,Type}]),
			  R = unicode:characters_to_list(
				io:get_chars(F,'',length(TestData)),unicode),
			  file:close(F),
			  R
		  end,
    ReadBomlessFile = fun({Type,File},DataLen,Dir) ->
			      {ok,F} = file:open(filename:join([Dir,File]),
						 [read,binary,
						  {encoding,Type}]),
			      R = unicode:characters_to_list(
				    io:get_chars(F,'',DataLen),unicode),
			      file:close(F),
			      R
		      end,
    ReadBomlessFileList = fun({Type,File},DataLen,Dir) ->
				  {ok,F} = file:open(filename:join([Dir,File]),
						     [read,
						      {encoding,Type}]),
				  R = io:get_chars(F,'',DataLen),
				  file:close(F),
				  R
			  end,
    ReadBomlessFileListLine = fun({Type,File},Dir) ->
				      {ok,F} = file:open(filename:join([Dir,File]),
							 [read,
							  {encoding,Type}]),
				      R = io:get_line(F,''),
				      file:close(F),
				      R
			      end,
    [TestData = ReadBomFile(F,DataDir) || F <- InternalBomFiles ],
    [ExternalTestData = ReadBomFile(F,DataDir) || F <- ExternalBomFiles ],
    [TestData = ReadBomlessFile(F,length(TestData),DataDir) || F <- AllNoBom ],
    [TestData = ReadBomlessFileList(F,length(TestData),DataDir) || F <- AllNoBom ],
    [TestData = ReadBomlessFileListLine(F,DataDir) || F <- AllNoBom ],

    BomDir = filename:join([PrivDir,"BOMDATA"]),
    BomlessDir = filename:join([PrivDir,"BOMLESSDATA"]),
    file:make_dir(BomDir),
    file:make_dir(BomlessDir),

    WriteBomFile = fun({Enc,File},Dir) ->
			   {ok,F} = file:open(filename:join([Dir,File]),
					      [write,binary]),
			   file:write(F,unicode:encoding_to_bom(Enc)),
			   io:setopts(F,[{encoding,Enc}]),
			   io:put_chars(F,TestData),
			   file:close(F),
			   ok
		   end,
    [ ok = WriteBomFile(F,BomDir) || F <- AllNoBom ],
    [TestData = ReadBomFile(F,BomDir) || {_,F} <- AllNoBom ],
    WriteBomlessFile = fun({Enc,File},TData,Dir) ->
			       {ok,F} = file:open(
					  filename:join([Dir,File]),
					  [write,binary,{encoding,Enc}]),
			       io:put_chars(F,TData),
			       file:close(F),
			       ok
		       end,
    [ ok = WriteBomlessFile(F,TestData,BomlessDir) || F <- AllNoBom ],
    [TestData = ReadBomlessFile(F,length(TestData),BomlessDir) || F <- AllNoBom ],
    [TestData = ReadBomlessFileList(F,length(TestData),BomlessDir) || F <- AllNoBom ],
    [TestData = ReadBomlessFileListLine(F,BomlessDir) || F <- AllNoBom ],

    CannotReadFile = fun({Enc,File},Dir) ->
			     %%io:format(standard_error,"~s\r\n",[filename:join([Dir,File])]),
			     {ok,F} = file:open(
					filename:join([Dir,File]),
					[read,binary,{encoding,Enc}]),
			     Enc2 = case Enc of
					utf8 ->
					    unicode;
					Tpl when is_tuple(Tpl) ->
					    Tpl;
					Atom when is_atom(Atom) ->
					    {Atom, big}
				    end,
			     {error, {no_translation,Enc2,latin1}} = 
				 file:read(F,10),
			     {error,terminated} = io:get_chars(F,'',10),
			     ok
		     end,
    [ ok = CannotReadFile(F,DataDir) || F <- AllNoBom ],
    [ ok = CannotReadFile(F,BomlessDir) || F <- AllNoBom ],
    [ ok = CannotReadFile(F,BomDir) || F <- AllNoBom ],

    [ ok = WriteBomlessFile(F,TestData2,BomlessDir) || F <- AllNoBom ],
    [TestData2 = ReadBomlessFile(F,length(TestData2),BomlessDir) || F <- AllNoBom ],
    [TestData2 = ReadBomlessFileList(F,length(TestData2),BomlessDir) || F <- AllNoBom ],
    [TestData2 = ReadBomlessFileListLine(F,BomlessDir) || F <- AllNoBom ],


    FailDir = filename:join([PrivDir,"FAIL"]),
    file:make_dir(FailDir),

    CannotWriteFile = fun({_Enc,File},Dir) ->
			      {ok,F} = file:open(
					 filename:join([Dir,File]),
					 [write,binary]),
			      {'EXIT', {no_translation,_}} =
				  (catch io:put_chars(F,TestData)),
			      {'EXIT', {terminated,_}} = (catch io:put_chars(F,TestData)),
			      ok
		      end,
    [ ok = CannotWriteFile(F,FailDir) || F <- AllNoBom ],

    case proplists:get_value(default_shell,Config) of
	old ->
	    ok;
	new ->
	    %% OK, time for the group_leaders...
	    rtnode([{putline,""},
		    {putline, "2."},
		    {getline, "2"},
		    {putline, "lists:keyfind(encoding,1,io:getopts())."},
		    {getline, "{encoding,latin1}"},
		    {putline, "io:format(\"~ts~n\",[[1024]])."},
		    {getline, "\\x{400}"},
		    {putline, "io:setopts([unicode])."},
		    {getline, "ok"},
		    {putline, "io:format(\"~ts~n\",[[1024]])."},
		    {getline,
		     binary_to_list(unicode:characters_to_binary(
				      [1024],unicode,utf8))}
		   ],[],"LC_CTYPE=\""++get_lc_ctype()++"\"; "
		   "export LC_CTYPE; ")
    end,
    rtnode([{putline,""},
	    {putline, "2."},
	    {getline_re, ".*2$"},
	    {putline, "lists:keyfind(encoding,1,io:getopts())."},
	    {getline_re, ".*{encoding,latin1}"},
	    {putline, "io:format(\"~ts~n\",[[1024]])."},
	    {getline_re, ".*\\\\x{400\\}"},
	    {putline, "io:setopts([{encoding,unicode}])."},
	    {getline_re, ".*ok"},
	    {putline, "io:format(\"~ts~n\",[[1024]])."},
	    {getline_re,
	     ".*"++binary_to_list(unicode:characters_to_binary(
				    [1024],unicode,utf8))}
	   ],[],"LC_CTYPE=\""++get_lc_ctype()++"\"; export LC_CTYPE; ",
	   " -oldshell "),

    ok.

%% Tests various unicode options on random generated files.
unicode_options_gen(Config) when is_list(Config) ->
    ct:timetrap({minutes,30}), %% valgrind needs a alot of time
    random:seed(1240, 900586, 553728),
    PrivDir = proplists:get_value(priv_dir, Config),
    AllModes = [utf8,utf16,{utf16,big},{utf16,little},
		utf32,{utf32,big},{utf32,little}],
    FSize = 9*1024,
    NumItersRead = 2,
    NumItersWrite = 2,
    Dir = filename:join(PrivDir, "GENDATA1"),
    file:make_dir(Dir),

    DoOneFile1 =
	fun(Encoding, N, M) ->
		?dbg({Encoding,M,N}),
		io:format("Read test: Encoding ~p, Chunk size ~p, Iteration ~p~n",[Encoding,M,N]),
		io:format(standard_error,
			  "Read test: Encoding ~p, Chunk size ~p, Iteration ~p\r\n",[Encoding,M,N]),
		Fname = filename:join(Dir,
				      "genfile_"++enc2str(Encoding)++
					  "_"++integer_to_list(N)),
		Ulist = random_unicode(FSize),
		Bin = unicode:characters_to_binary(Ulist, utf8, Encoding),
		ok = file:write_file(Fname, Bin),

		Read1 = fun(FD) -> io:get_line(FD, '') end,
		Res1 = read_whole_file(Fname,
				       [read,read_ahead,{encoding,Encoding}],
				       Read1),

		Read2 = fun(FD) -> io:get_chars(FD, '', M) end,
		Res2 = read_whole_file(Fname,
				       [read,binary,
					read_ahead,{encoding,Encoding}],
				       Read2),

		Read3 = fun(FD) ->
				case io:fread(FD, '', "~ts") of
				    {ok,D} -> D;
				    Other -> Other end
			end,
		Res3 = read_whole_file(Fname,
				       [read,binary,
					read_ahead,{encoding,Encoding}],
				       Read3),

		Read4 = fun(FD) ->
				case io:fread(FD, '', "~ts") of
				    {ok,D} -> D;
				    Other -> Other end
			end,
		Res4 = read_whole_file(Fname,
				       [read,read_ahead,{encoding,Encoding}],
				       Read4),

		Ulist2 = [X || X <- Ulist, X =/= $\n, X =/= $\s],
		Ulist3 = [X || X <- Ulist, X =/= $\n],
		Ulist = done(Res1),
		Ulist = done(Res2),
		Ulist2 = done(Res3),
		Ulist3 = done(Res4),

		file:delete(Fname)
	end,
    [ [ [ DoOneFile1(E, N, M) || E <- AllModes ] ||
	  M <- [10,1000,128,1024,8192,8193] ] ||
	N <- lists:seq(1, NumItersRead) ],

    DoOneFile2 =
	fun(Encoding,N,M) ->
		?dbg({Encoding,M,N}),
		io:format("Write test: Encoding ~p, Chunk size ~p, Iteration ~p~n",[Encoding,M,N]),
		io:format(standard_error,
			  "Write test: Encoding ~p, Chunk size ~p, Iteration ~p\r\n",[Encoding,M,N]),
		Fname = filename:join(Dir,
				      "genfile_"++enc2str(Encoding)++
					  "_"++integer_to_list(N)),
		Ulist = random_unicode(FSize),

		Res1 = write_read_file(Fname, 1,
				       [write],
				       Encoding,
				       fun(FD) -> io:put_chars(FD, Ulist) end),

		Res2 = write_read_file(Fname, 2,
				       [write,binary],
				       Encoding,
				       fun(FD) -> io:put_chars(FD, Ulist) end),

		Fun3 = fun(FD) ->
			       _ = [io:format(FD, "~tc", [C]) || C <- Ulist],
			       ok
		       end,
		Res3 = write_read_file(Fname, 3,
				       [write],
				       Encoding,
				       Fun3),

		Fun4 = fun(FD) ->
			       io:put_chars(FD,
					    unicode:characters_to_binary(Ulist))
		       end,
		Res4 = write_read_file(Fname, 4,
				       [write],
				       Encoding,
				       Fun4),

		LL = string:tokens(Ulist, "\n"),
		Fun5 = fun(FD) ->
			       _ = [io:format(FD, "~ts", [L]) || L <- LL],
			       ok
		       end,
		Res5 = write_read_file(Fname, 5,
				       [write],
				       Encoding,
				       Fun5),

		Ulist2 = lists:flatten(LL),
		ResBin = done(Res1),
		ResBin = done(Res2),
		ResBin = done(Res3),
		ResBin = done(Res4),
		Ulist = unicode:characters_to_list(ResBin, Encoding),

		ResBin2 = done(Res5),
		Ulist2 = unicode:characters_to_list(ResBin2, Encoding),

		ok
	end,
    [ [ [ DoOneFile2(E, N, M) || E <- AllModes ] ||
	  M <- [10,1000,128,1024,8192,8193] ] ||
	N <- lists:seq(1, NumItersWrite) ],
    ok.

read_whole_file(Fname, Options, Fun) ->
    do(fun() ->
	       do_read_whole_file(Fname, Options, Fun)
       end).

do_read_whole_file(Fname, Options, Fun) ->
    {ok,F} = file:open(Fname, Options),
    Res = do_read_whole_file_1(Fun, F),
    ok = file:close(F),
    unicode:characters_to_list(Res, unicode).

do_read_whole_file_1(Fun, F) ->
    case Fun(F) of
	eof ->
	    [];
	{error,Error} ->
	    receive after 10000 -> ok end,
	    exit(Error);
	Other ->
	    [Other|do_read_whole_file_1(Fun, F)]
    end.

write_read_file(Fname0, N, Options, Enc, Writer) ->
    Fname = Fname0 ++ "_" ++ integer_to_list(N),
    do(fun() ->
	       do_write_read_file(Fname, Options, Enc, Writer)
       end).

do_write_read_file(Fname, Options, Encoding, Writer) ->
    {ok,F} = file:open(Fname, [{encoding,Encoding}|Options]),
    Writer(F),
    ok = file:close(F),
    {ok,Bin} = file:read_file(Fname),
    ok = file:delete(Fname),
    Bin.

enc2str(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
enc2str({A1,A2}) when is_atom(A1), is_atom(A2) ->
    atom_to_list(A1)++"_"++atom_to_list(A2).


random_unicode(0) ->
    [];
random_unicode(N) ->
    %% Favour large unicode and make linebreaks
    X = case random:uniform(20) of
	    A when A =< 1 -> $\n;
	    A0 when A0 =< 3 -> random:uniform(16#10FFFF);
	    A1 when A1 =< 6 -> random:uniform(16#10FFFF - 16#7F) + 16#7F;
	    A2 when A2 =< 12 -> random:uniform(16#10FFFF - 16#7FF) + 16#7FF;
	    _ -> random:uniform(16#10FFFF - 16#FFFF) + 16#FFFF
	end,
    case X of
	Inv1 when Inv1 >= 16#D800, Inv1 =< 16#DFFF;
	          Inv1 =:= 16#FFFE; 
	          Inv1 =:= 16#FFFF ->
	    random_unicode(N);
	_ ->
	    [X | random_unicode(N-1)]
    end.


%% Test variants with binary option.
binary_options(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    PrivDir = proplists:get_value(priv_dir,Config),
    TestData = unicode:characters_to_binary(
		 [1090,1093,1077,32,1073,1080,1075,32,
		  1088,1077,1076,32,1092,1086,1100,32,1093,
		  1072,1089,32,1089,1086,1100,32,932,951,949,
		  32,946,953,947,32,961,949,948,32,
		  963,959,967,32,945,961,949,32,966,959,967,949,963]),
    <<First10:10/binary,Second10:10/binary,_/binary>> = TestData,
    First10List = binary_to_list(First10),
    Second10List = binary_to_list(Second10),
    TestFile = filename:join([DataDir, "testdata_utf8.dat"]),
    {ok, F} = file:open(TestFile,[read]),
    {ok, First10List} = file:read(F,10),
    io:setopts(F,[binary]),
    {ok, Second10} = file:read(F,10),
    file:close(F),
    {ok, F2} = file:open(TestFile,[read,binary]),
    {ok, First10} = file:read(F2,10),
    io:setopts(F2,[list]),
    {ok, Second10List} = file:read(F2,10),
    file:position(F2,0),
    First10List = io:get_chars(F2,'',10),
    io:setopts(F2,[binary]),
    Second10 = unicode:characters_to_binary(io:get_chars(F2,'',10),unicode,latin1),
    file:close(F2),
    LineBreakFileName =  filename:join([PrivDir, "testdata.dat"]),
    LineBreakTestData = <<TestData/binary,$\n>>,
    LineBreakTestDataList = binary_to_list(LineBreakTestData),
    file:write_file(LineBreakFileName,[LineBreakTestData,LineBreakTestData,LineBreakTestData,TestData]),
    {ok, F3} = file:open(LineBreakFileName,[read]),
    LineBreakTestDataList = io:get_line(F3,''),
    io:setopts(F3,[binary]),
    LineBreakTestData =  unicode:characters_to_binary(io:get_line(F3,''),unicode,latin1),
    io:setopts(F3,[list]),
    LineBreakTestDataList = io:get_line(F3,''),
    io:setopts(F3,[binary]),
    TestData = unicode:characters_to_binary(io:get_line(F3,''),unicode,latin1),
    eof = io:get_line(F3,''),
    file:close(F3),

    %% OK, time for the group_leaders...
    case proplists:get_value(default_shell,Config) of
	old ->
	    ok;
	new ->
	    rtnode([{putline, "2."},
		    {getline, "2"},
		    {putline, "lists:keyfind(binary,1,io:getopts())."},
		    {getline, "{binary,false}"},
		    {putline, "io:get_line('')."},
		    {putline, "hej"},
		    {getline, "\"hej\\n\""},
		    {putline, "io:setopts([{binary,true},unicode])."},
		    {getline, "ok"},
		    {putline, "io:get_line('')."},
		    {putline, "hej"},
		    {getline, "<<\"hej\\n\">>"},
		    {putline, "io:get_line('')."},
		    {putline, binary_to_list(<<"\345\344\366"/utf8>>)},
		    {getline, "<<\""++binary_to_list(<<"\345\344\366"/utf8>>)++"\\n\"/utf8>>"}
		   ],[])
    end,
    %% And one with oldshell
    rtnode([{putline, "2."},
	    {getline_re, ".*2$"},
	    {putline, "lists:keyfind(binary,1,io:getopts())."},
	    {getline_re, ".*{binary,false}"},
	    {putline, "io:get_line('')."},
	    {putline, "hej"},
	    {getline_re, ".*\"hej\\\\n\""},
	    {putline, "io:setopts([{binary,true},unicode])."},
	    {getline_re, ".*ok"},
	    {putline, "io:get_line('')."},
	    {putline, "hej"},
	    {getline_re, ".*<<\"hej\\\\n\">>"},
	    {putline, "io:get_line('')."},
	    {putline, binary_to_list(<<"\345\344\366"/utf8>>)},
	    {getline_re, ".*<<\""++binary_to_list(<<"\345\344\366"/utf8>>)++"\\\\n\"/utf8>>"}
	   ],[],[],"-oldshell"),
    ok.




answering_machine1(OthNode,OthReg,Me) ->
    TestDataLine1 = [229,228,246],
    TestDataUtf = binary_to_list(unicode:characters_to_binary(TestDataLine1)),
    rtnode([{putline,""},
	    {putline, "2."},
	    {getline, "2"},
	    {putline, "{"++OthReg++","++OthNode++"} ! group_leader()."},
	    {getline, "<"},
	    %% get_line
	    {getline_re, ".*Prompt"},
	    {putline, "Hej"},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, "Hej"},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataLine1},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataLine1},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataUtf},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataUtf},
	    {getline_re, ".*Okej"},
	    %% get_chars
	    {getline_re, ".*Prompt"},
	    {putline, "Hej"},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, "Hej"},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataLine1},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataLine1},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataUtf},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataUtf},
	    {getline_re, ".*Okej"},
	    %% fread
	    {getline_re, ".*Prompt"},
	    {putline, "Hej"},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, "Hej"},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataLine1},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataLine1},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataUtf},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataUtf},
	    {getline_re, ".*Okej"}

	   ],Me,"LC_CTYPE=\""++get_lc_ctype()++"\"; export LC_CTYPE; "),
    O = list_to_atom(OthReg),
    O ! {self(),done},
    ok.

answering_machine2(OthNode,OthReg,Me) ->
    TestDataLine1 = [229,228,246],
    TestDataUtf = binary_to_list(unicode:characters_to_binary(TestDataLine1)),
    rtnode([{putline,""},
	    {putline, "2."},
	    {getline, "2"},
	    {putline, "{"++OthReg++","++OthNode++"} ! group_leader()."},
	    {getline_re, ".*<[0-9].*"},
	    %% get_line
	    {getline_re, ".*Prompt"},
	    {putline, "Hej"},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, "Hej"},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataLine1},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataLine1},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataUtf},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataUtf},
	    {getline_re, ".*Okej"},
	    %% get_chars
	    {getline_re, ".*Prompt"},
	    {putline, "Hej"},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, "Hej"},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataLine1},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataLine1},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataUtf},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataUtf},
	    {getline_re, ".*Okej"},
	    %% fread
	    {getline_re, ".*Prompt"},
	    {putline, "Hej"},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, "Hej"},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataLine1},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataLine1},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataUtf},
	    {getline_re, ".*Okej"},
	    {getline_re, ".*Prompt"},
	    {putline, TestDataUtf},
	    {getline_re, ".*Okej"}

	   ],Me,"LC_CTYPE=\""++get_lc_ctype()++"\"; export LC_CTYPE; "," -oldshell "),
    O = list_to_atom(OthReg),
    O ! {self(),done},
    ok.


%% Test various modes when reading from the group leade from another machine.
read_modes_ogl(Config) when is_list(Config) -> 
    case get_progs() of
	{error,Reason} ->
	    {skipped,Reason};
	_ ->
	    read_modes_gl_1(Config,answering_machine2)
    end.

%% Test various modes when reading from the group leade from another machine.
read_modes_gl(Config) when is_list(Config) -> 
    case {get_progs(),proplists:get_value(default_shell,Config)} of
	{{error,Reason},_} ->
	    {skipped,Reason};
	{_,old} ->
	    {skipper,"No new shell"};
	_ ->
	    read_modes_gl_1(Config,answering_machine1)
    end.

read_modes_gl_1(_Config,Machine) ->
    TestDataLine1 = [229,228,246],
    TestDataLine1BinUtf = unicode:characters_to_binary(TestDataLine1),
    TestDataLine1BinLatin = list_to_binary(TestDataLine1),

    {ok,N2List} = create_nodename(),
    MyNodeList = atom2list(node()),
    register(io_proto_suite,self()),
    AM1 = spawn(?MODULE,Machine,
		[MyNodeList, "io_proto_suite", N2List]),

    GL = receive X when is_pid(X) -> X end,
    ?dbg({group_leader,X}),
    %% get_line
    receive after 500 -> ok end, % Dont clash with the new shell...
    "Hej\n" = io:get_line(GL,"Prompt\n"),
    io:setopts(GL,[binary]),
    io:format(GL,"Okej~n",[]),
    <<"Hej\n">> = io:get_line(GL,"Prompt\n"),
    io:setopts(GL,[{encoding,latin1}]),
    io:format(GL,"Okej~n",[]),
    TestDataLine1BinLatin = chomp(io:request(GL,{get_line,latin1,"Prompt\n"})),
    io:format(GL,"Okej~n",[]),
    TestDataLine1BinUtf = chomp(io:get_line(GL,"Prompt\n")),
    io:setopts(GL,[{encoding,unicode}]),

    io:format(GL,"Okej~n",[]),
    TestDataLine1BinLatin = chomp(io:request(GL,{get_line,latin1,"Prompt\n"})),
    io:format(GL,"Okej~n",[]),
    TestDataLine1BinUtf = chomp(io:get_line(GL,"Prompt\n")),
    io:setopts(GL,[list]),
    io:format(GL,"Okej~n",[]),

    %%get_chars
    "Hej" = io:get_chars(GL,"Prompt\n",3),
    io:setopts(GL,[binary]),
    io:format(GL,"Okej~n",[]),
    <<"Hej">> = io:get_chars(GL,"Prompt\n",3),
    io:setopts(GL,[{encoding,latin1}]),
    io:format(GL,"Okej~n",[]),
    TestDataLine1BinLatin = io:request(GL,{get_chars,latin1,"Prompt\n",3}),
    io:format(GL,"Okej~n",[]),
    TestDataLine1BinUtf = io:get_chars(GL,"Prompt\n",3),
    io:setopts(GL,[{encoding,unicode}]),

    io:format(GL,"Okej~n",[]),
    TestDataLine1BinLatin = io:request(GL,{get_chars,latin1,"Prompt\n",3}),
    io:format(GL,"Okej~n",[]),
    TestDataLine1BinUtf = io:get_chars(GL,"Prompt\n",3),
    io:setopts(GL,[list]),
    io:format(GL,"Okej~n",[]),
    %%fread
    {ok,["Hej"]} = io:fread(GL,"Prompt\n","~s"),
    io:setopts(GL,[binary]),
    io:format(GL,"Okej~n",[]),
    {ok,["Hej"]} = io:fread(GL,"Prompt\n","~s"),
    io:setopts(GL,[{encoding,latin1}]),
    io:format(GL,"Okej~n",[]),
    {ok,[TestDataLine1]} = io:fread(GL,"Prompt\n","~s"),
    io:format(GL,"Okej~n",[]),
    {ok,[TestDataLine1]} = io:fread(GL,"Prompt\n","~s"),
    io:setopts(GL,[{encoding,unicode}]),
    io:format(GL,"Okej~n",[]),
    {ok,[TestDataLine1]} = io:fread(GL,"Prompt\n","~s"),
    io:format(GL,"Okej~n",[]),
    {ok,[TestDataLine1]} = io:fread(GL,"Prompt\n","~s"),
    io:setopts(GL,[list]),
    io:format(GL,"Okej~n",[]),


    receive
	{AM1,done} ->
	    ok
    after 5000 ->
	    exit(timeout)
    end,
    ok.


%% Test behaviour when reading broken Unicode files
broken_unicode(Config) when is_list(Config) ->
    Dir = proplists:get_value(priv_dir,Config),
    Latin1Name = filename:join([Dir,"latin1_data_file.dat"]),
    Utf8Name = filename:join([Dir,"utf8_data_file.dat"]),
    Latin1Data = iolist_to_binary(lists:duplicate(10,lists:seq(0,255)++[255,255,255])),
    Utf8Data = unicode:characters_to_binary(
		 lists:duplicate(10,lists:seq(0,255))),
    file:write_file(Latin1Name,Latin1Data),
    file:write_file(Utf8Name,Utf8Data),
    [ latin1 = heuristic_encoding_file2(Latin1Name,N,utf8) || N <- lists:seq(1,100)++[1024,2048,10000]],
    [ utf8 = heuristic_encoding_file2(Utf8Name,N,utf8) || N <- lists:seq(1,100)++[1024,2048,10000]],
    [ latin1 = heuristic_encoding_file2(Latin1Name,N,utf16) || N <- lists:seq(1,100)++[1024,2048,10000]],
    [ latin1 = heuristic_encoding_file2(Latin1Name,N,utf32) || N <- lists:seq(1,100)++[1024,2048,10000]],
    ok.


%%
%% From the cookbook, more or less
heuristic_encoding_file2(FileName,Chunk,Enc) ->
    {ok,F} = file:open(FileName,[read,binary,{encoding,Enc}]),
    loop_through_file2(F,io:get_chars(F,'',Chunk),Chunk,Enc).

loop_through_file2(_,eof,_,Enc) ->
    Enc;
loop_through_file2(_,{error,_Err},_,_) ->
    latin1;
loop_through_file2(F,Bin,Chunk,Enc) when is_binary(Bin) ->
    loop_through_file2(F,io:get_chars(F,'',Chunk),Chunk,Enc).



%% Test eof before newline on stdin when erlang is in pipe.
eof_on_pipe(Config) when is_list(Config) ->
    case {get_progs(),os:type()} of
	{{error,Reason},_} ->
	    {skipped,Reason};
	{{_,_,Erl},{unix,linux}} -> 
	    %% Not even Linux is reliable - echo can be both styles
	    try
		EchoLine = case os:cmd("echo -ne \"test\\ntest\"") of
			       "test\ntest" ->
				   "echo -ne \"a\\nbu\" | ";
			       _ ->
				   case os:cmd("echo \"test\\ntest\\c\"") of
				       "test\ntest" ->
					   "echo \"a\\nbu\\c\" | ";
				       _ ->
					   throw(skip)
				   end
			   end,
		CommandLine1 = EchoLine ++
		    "\""++Erl++"\" -noshell -eval  "
		    "'io:format(\"~p\",[io:get_line(\"\")]),"
		    "io:format(\"~p\",[io:get_line(\"\")]),"
		    "io:format(\"~p\",[io:get_line(\"\")]).' -run init stop",
		case os:cmd(CommandLine1) of
		    "\"a\\n\"\"bu\"eof" ->
			ok;
		    Other1 ->
			exit({unexpected1,Other1})
		end,
		CommandLine2 = EchoLine ++
		    "\""++Erl++"\" -noshell -eval  "
		    "'io:setopts([binary]),io:format(\"~p\",[io:get_line(\"\")]),"
		    "io:format(\"~p\",[io:get_line(\"\")]),"
		    "io:format(\"~p\",[io:get_line(\"\")]).' -run init stop",
		case os:cmd(CommandLine2) of
		    "<<\"a\\n\">><<\"bu\">>eof" ->
			ok;
		    Other2 ->
			exit({unexpected2,Other2})
		end
	    catch
		throw:skip ->
		    {skipped,"unsupported echo program"}
	    end;
	{_,_} ->
	    {skipped,"Only on linux"}
    end.


%%
%% Tool for running interactive shell (stolen from the kernel
%% test suite interactive_shell_SUITE)
%%
-undef(line).
-define(line,).
rtnode(C,N) ->
    rtnode(C,N,[]).
rtnode(Commands,Nodename,ErlPrefix) ->
    rtnode(Commands,Nodename,ErlPrefix,[]).
rtnode(Commands,Nodename,ErlPrefix,Extra) ->
    case get_progs() of
	{error,_Reason} ->
	    {skip,"No runerl present"};
	{RunErl,ToErl,Erl} ->
	    case create_tempdir() of
		{error, Reason2} ->
		    {skip, Reason2};
		Tempdir ->
		    SPid = start_runerl_node(RunErl, ErlPrefix++
						 "\\\""++Erl++"\\\"",
					     Tempdir, Nodename, Extra),
		    CPid = start_toerl_server(ToErl, Tempdir),
		    put(getline_skipped, []),
		    Res = (catch get_and_put(CPid, Commands, 1)),
		    case stop_runerl_node(CPid) of
			{error,_} ->
			    CPid2 = start_toerl_server(ToErl, Tempdir),
			    put(getline_skipped, []),
			    ok = get_and_put
				   (CPid2,
				    [{putline,[7]},
				     {sleep,
				      timeout(short)},
				     {putline,""},
				     {getline," -->"},
				     {putline,"s"},
				     {putline,"c"},
				     {putline,""}], 1),
			    stop_runerl_node(CPid2);
			_ ->
			    ok
		    end,
		    wait_for_runerl_server(SPid),
		    ok = ?RM_RF(Tempdir),
		    ok = Res
	    end
    end.

timeout(long) ->
    2 * timeout(normal);
timeout(short) ->
    timeout(normal) div 10;
timeout(normal) ->
    10000 * test_server:timetrap_scale_factor().


%% start_noshell_node(Name) ->
%%     PADir =  filename:dirname(code:which(?MODULE)),
%%     {ok, Node} = test_server:start_node(Name,slave,[{args," -noshell -pa "++
%% 						     PADir++" "}]),
%%     Node.
%% stop_noshell_node(Node) ->
%%     test_server:stop_node(Node).

-ifndef(debug).
rm_rf(Dir) ->
    try
	{ok,List} = file:list_dir(Dir),
	Files = [filename:join([Dir,X]) || X <- List],
	[case file:list_dir(Y) of
	     {error, enotdir} ->
		 ok = file:delete(Y);
	     _ ->
		 ok = rm_rf(Y)
	 end || Y <- Files],
	ok = file:del_dir(Dir),
	ok
    catch
	_:Exception -> {error, {Exception,Dir}}
    end.
-endif.       

get_and_put(_CPid,[],_) ->
    ok;
get_and_put(CPid, [{sleep, X}|T],N) ->
    ?dbg({sleep, X}),
    receive
    after X ->
	    get_and_put(CPid,T,N+1)
    end;
get_and_put(CPid, [{getline_pred,Pred,Msg}|T]=T0, N)
  when is_function(Pred) ->
    ?dbg({getline, Match}),
    CPid ! {self(), {get_line, timeout(normal)}},
    receive
	{get_line, timeout} ->
	    error_logger:error_msg("~p: getline timeout waiting for \"~s\" "
				   "(command number ~p, skipped: ~p)~n",
				   [?MODULE,Msg,N,get(getline_skipped)]),
	    {error, timeout};
	{get_line, Data} ->
	    ?dbg({data,Data}),
	    case Pred(Data) of
		yes ->
		    put(getline_skipped, []),
		    get_and_put(CPid, T,N+1);
		no ->
		    error_logger:error_msg("~p: getline match failure "
					   "\"~s\" "
					   "(command number ~p)\n",
					   [?MODULE,Msg,N]),
		    {error, no_match};
		maybe ->
		    List = get(getline_skipped),
		    put(getline_skipped, List ++ [Data]),
		    get_and_put(CPid, T0, N)
	    end
    end;
get_and_put(CPid, [{getline, Match}|T],N) ->
    ?dbg({getline, Match}),
    F = fun(Data) ->
		case lists:prefix(Match, Data) of
		    true -> yes;
		    false -> maybe
		end
	end,
    get_and_put(CPid, [{getline_pred,F,Match}|T], N);
get_and_put(CPid, [{getline_re, Match}|T],N) ->
    F = fun(Data) ->
		case re:run(Data, Match, [{capture,none}]) of
		    match -> yes;
		    _ -> maybe
		end
	end,
    get_and_put(CPid, [{getline_pred,F,Match}|T], N);
get_and_put(CPid, [{putline_raw, Line}|T],N) ->
    ?dbg({putline_raw, Line}),
    CPid ! {self(), {send_line, Line}},
    Timeout = timeout(normal),
    receive
	{send_line, ok} ->
	    get_and_put(CPid, T,N+1)
    after Timeout ->
	    error_logger:error_msg("~p: putline_raw timeout (~p) sending "
				   "\"~s\" (command number ~p)~n",
				   [?MODULE, Timeout, Line, N]),
	    {error, timeout}
    end;

get_and_put(CPid, [{putline, Line}|T],N) ->
    ?dbg({putline, Line}),
    CPid ! {self(), {send_line, Line}},
    Timeout = timeout(normal),
    receive
	{send_line, ok} ->
	    get_and_put(CPid, [{getline, []}|T],N)
    after Timeout ->
	    error_logger:error_msg("~p: putline timeout (~p) sending "
				   "\"~s\" (command number ~p)~n[~p]~n",
				   [?MODULE, Timeout, Line, N,get()]),
	    {error, timeout}
    end.

wait_for_runerl_server(SPid) ->
    Ref = erlang:monitor(process, SPid), 
    Timeout = timeout(long),
    receive
	{'DOWN', Ref, process, SPid, _} ->
	    ok
    after Timeout ->
	    {error, timeout}
    end.



stop_runerl_node(CPid) ->
    Ref = erlang:monitor(process, CPid),
    CPid ! {self(), kill_emulator},
    Timeout = timeout(long),
    receive
	{'DOWN', Ref, process, CPid, noproc} ->
	    ok;
	{'DOWN', Ref, process, CPid, normal} ->
	    ok;
	{'DOWN', Ref, process, CPid, {error, Reason}} ->
	    {error, Reason}
    after Timeout ->
	    {error, timeout}
    end.

get_progs() ->
    case os:type() of
	{unix,freebsd} ->
	    {error,"cant use run_erl on freebsd"};
	{unix,openbsd} ->
	    {error,"cant use run_erl on openbsd"};
	{unix,_} ->
	    case os:find_executable("run_erl") of
		RE when is_list(RE) ->
		    case  os:find_executable("to_erl") of
			TE when is_list(TE) ->
			    case os:find_executable("erl") of
				E when is_list(E) ->
				    {RE,TE,E};
				_ ->
				    {error, "Could not find erl command"}
			    end;
			_ ->
			    {error, "Could not find to_erl command"}
		    end;
		_ ->
		    {error, "Could not find run_erl command"}
	    end;
	_ ->
	    {error, "Not a unix OS"}
    end.

create_tempdir() ->
    create_tempdir(filename:join(["/tmp","rtnode"++os:getpid()]),$A).

create_tempdir(Dir,X) when X > $Z, X < $a ->
    create_tempdir(Dir,$a);
create_tempdir(Dir,X) when X > $z -> 
    Estr = lists:flatten(
	     io_lib:format("Unable to create ~s, reason eexist",
			   [Dir++[$z]])),
    {error, Estr};
create_tempdir(Dir0, Ch) ->
    %% Expect fairly standard unix.
    Dir = Dir0++[Ch],
    case file:make_dir(Dir) of
	{error, eexist} ->
	    create_tempdir(Dir0, Ch+1);
	{error, Reason} ->
	    Estr = lists:flatten(
		     io_lib:format("Unable to create ~s, reason ~p",
				   [Dir,Reason])),
	    {error,Estr};
	ok ->
	    Dir
    end.

create_nodename() ->
    create_nodename($A).

create_nodename(X) when X > $Z, X < $a ->
    create_nodename($a);
create_nodename(X) when X > $z -> 
    {error,out_of_nodenames};
create_nodename(X) ->
    NN = "rtnode"++os:getpid()++[X],
    case file:read_file_info(filename:join(["/tmp",NN])) of
	{error,enoent} ->
	    Host = lists:nth(2,string:tokens(atom_to_list(node()),"@")),
	    {ok,NN++"@"++Host};
	_ ->
	    create_nodename(X+1)
    end.


start_runerl_node(RunErl,Erl,Tempdir,Nodename,Extra) ->
    XArg = case Nodename of
	       [] ->
		   [];
	       _ ->
		   " -sname "++(if is_atom(Nodename) -> atom_to_list(Nodename);
				   true -> Nodename
				end)++
		       " -setcookie "++atom_to_list(erlang:get_cookie())
	   end,
    XXArg = case Extra of
		[] ->
		    [];
		_ ->
		    " "++Extra
	    end,
    spawn(fun() ->
		  ?dbg("\""++RunErl++"\" "++Tempdir++"/ "++Tempdir++
			   " \""++Erl++XArg++XXArg++"\""),
		  os:cmd("\""++RunErl++"\" "++Tempdir++"/ "++Tempdir++
			     " \""++Erl++XArg++XXArg++"\"")
	  end).

start_toerl_server(ToErl,Tempdir) ->
    Pid = spawn(?MODULE,toerl_server,[self(),ToErl,Tempdir]),
    receive
	{Pid,started} ->
	    Pid;
	{Pid,error,Reason} ->
	    {error,Reason}
    end.

try_to_erl(_Command, 0) ->
    {error, cannot_to_erl};
try_to_erl(Command, N) ->
    ?dbg({?LINE,N}),
    Port = open_port({spawn, Command},[eof,{line,1000}]),
    Timeout = timeout(normal) div 2,
    receive
	{Port, eof} -> 	
	    receive after Timeout ->
			    ok
		    end,
	    try_to_erl(Command, N-1)
    after Timeout ->
	    ?dbg(Port),
	    Port
    end.

toerl_server(Parent,ToErl,Tempdir) ->
    Port = try_to_erl("\""++ToErl++"\" "++Tempdir++"/ 2>/dev/null",8),
    case Port of
	P when is_port(P) ->
	    Parent ! {self(),started};
	{error,Other} ->
	    Parent ! {self(),error,Other},
	    exit(Other)
    end,
    case toerl_loop(Port,[]) of
	normal ->
	    ok;
	{error, Reason} ->
	    error_logger:error_msg("toerl_server exit with reason ~p~n",
				   [Reason]),
	    exit(Reason)
    end.

toerl_loop(Port,Acc) ->
    ?dbg({toerl_loop, Port, Acc}),
    receive
	{Port,{data,{Tag0,Data}}} when is_port(Port) ->
	    ?dbg({?LINE,Port,{data,{Tag0,Data}}}),
	    case Acc of
		[{noeol,Data0}|T0] ->
		    toerl_loop(Port,[{Tag0, Data0++Data}|T0]);
		_ ->
		    toerl_loop(Port,[{Tag0,Data}|Acc])
	    end;
	{Pid,{get_line,Timeout}} ->
	    case Acc of
		[] ->
		    case get_data_within(Port,Timeout,[]) of
			timeout ->
			    Pid ! {get_line, timeout},
			    toerl_loop(Port,[]);
			{noeol,Data1} ->
			    Pid ! {get_line, timeout},
			    toerl_loop(Port,[{noeol,Data1}]);
			{eol,Data2} ->
			    Pid ! {get_line, Data2}, 
			    toerl_loop(Port,[])
		    end;
		[{noeol,Data3}] ->
		    case get_data_within(Port,Timeout,Data3) of
			timeout ->
			    Pid ! {get_line, timeout},
			    toerl_loop(Port,Acc);
			{noeol,Data4} ->
			    Pid ! {get_line, timeout},
			    toerl_loop(Port,[{noeol,Data4}]);
			{eol,Data5} ->
			    Pid ! {get_line, Data5},
			    toerl_loop(Port,[])
		    end;
		List ->
		    {NewAcc,[{eol,Data6}]} = lists:split(length(List)-1,List),
		    Pid ! {get_line,Data6},
		    toerl_loop(Port,NewAcc)
	    end;
	{Pid, {send_line, Data7}} ->
	    Port ! {self(),{command, Data7++"\n"}},
	    Pid ! {send_line, ok},
	    toerl_loop(Port,Acc);
	{_Pid, kill_emulator} ->
	    Port ! {self(),{command, "init:stop().\n"}},
	    Timeout1 = timeout(long),
	    receive
		{Port,eof} ->
		    normal
	    after Timeout1 ->
		    {error, kill_timeout}
	    end;
	{Port, eof} ->
	    {error, unexpected_eof};
	Other ->
	    {error, {unexpected, Other}}
    end.

millistamp() ->
    erlang:monotonic_time(millisecond).

get_data_within(Port, X, Acc) when X =< 0 ->
    ?dbg({get_data_within, X, Acc, ?LINE}),
    receive
	{Port,{data,{Tag0,Data}}} ->
	    ?dbg({?LINE,Port,{data,{Tag0,Data}}}),
	    {Tag0, Acc++Data}
    after 0 ->
	    case Acc of
		[] ->
		    timeout;
		Noeol ->
		    {noeol,Noeol}
	    end
    end;


get_data_within(Port, Timeout, Acc) ->	
    ?dbg({get_data_within, Timeout, Acc, ?LINE}),
    T1 = millistamp(),
    receive 
	{Port,{data,{noeol,Data}}} ->
	    ?dbg({?LINE,Port,{data,{noeol,Data}}}),
	    Elapsed = millistamp() - T1 + 1,
	    get_data_within(Port, Timeout - Elapsed, Acc ++ Data); 
	{Port,{data,{eol,Data1}}} ->
	    ?dbg({?LINE,Port,{data,{eol,Data1}}}),
	    {eol, Acc ++ Data1}
    after Timeout ->
	    timeout
    end.

get_default_shell() ->
    Match = fun(Data) ->
		    case lists:prefix("undefined", Data) of
			true ->
			    yes;
			false ->
			    case re:run(Data, "<\\d+[.]\\d+[.]\\d+>",
					[{capture,none}]) of
				match -> no;
				_ -> maybe
			    end
		    end
	    end,
    try
	rtnode([{putline,""},
		{putline, "whereis(user_drv)."},
		{getline_pred, Match, "matching of user_drv pid"}], []),
	old
    catch _E:_R ->
	    ?dbg({_E,_R}),
	    new
    end.

%%
%% Test I/O-server
%%

start_io_server_proxy() ->
    spawn_link(?MODULE,io_server_proxy,[#state{}]).

proxy_getall(Pid) ->
    req(Pid,{self(),getall}).
proxy_setnext(Pid,Data) when is_list(Data) ->
    req(Pid,{self(),next,Data}).
proxy_quit(Pid) ->
    req(Pid,{self(),quit}).

req(Pid,Mess) ->
    Pid ! Mess,
    receive
	{Pid, Answer} ->
	    Answer
    after 5000 ->
	    exit(timeout)
    end.

io_server_proxy(State) -> 
    receive
        {io_request, From, ReplyAs, Request} ->
            case request(Request,State) of
                {Tag, Reply, NewState} when Tag =:= ok; Tag =:= error ->
                    reply(From, ReplyAs, Reply),
                    io_server_proxy(NewState);
                {stop, Reply, _NewState} ->
                    reply(From, ReplyAs, Reply),
                    exit(Reply)
            end;
        %% Private message
        {From, next, Data} ->
            From ! {self(), ok},
            io_server_proxy(State#state{nxt = Data});
        {From, getall} ->
            From ! {self(), lists:reverse(State#state.q)},
            io_server_proxy(State#state{q=[]});
        {From, quit} ->
            From ! {self(), lists:reverse(State#state.q)},
	    ok;
        _Unknown ->
            io_server_proxy(State)
    end.

reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

request({put_chars, Encoding, Chars}, State) ->
    {ok, ok, State#state{q=[{put_chars, Encoding, Chars} | State#state.q ]}};
request({put_chars, Encoding, Module, Function, Args}, State) ->
    {ok, ok, State#state{q=[{put_chars, Encoding, Module, Function, Args} | 
			    State#state.q ]}};
request({put_chars,Chars}, State) ->
    {ok, ok, State#state{q=[{put_chars, Chars} | State#state.q ]}};
request({put_chars,M,F,As}, State) ->
    {ok, ok, State#state{q=[{put_chars, M,F,As} | State#state.q ]}};
request({get_until, Encoding, Prompt, M, F, As}, State) ->
    {ok, convert(State#state.nxt, Encoding, State#state.mode), State#state{nxt = eof, q = [{get_until, Encoding, Prompt, M, F, As} | State#state.q]}};
request({get_chars, Encoding, Prompt, N}, State) ->
    {ok, convert(State#state.nxt, Encoding, State#state.mode), State#state{nxt = eof, 
									   q = [{get_chars, Encoding, Prompt, N} |
										State#state.q]}};
request({get_line, Encoding, Prompt}, State) ->
    {ok, convert(State#state.nxt, Encoding, State#state.mode), 
     State#state{nxt = eof, 
		 q = [{get_line, Encoding, Prompt} | 
		      State#state.q]}};
request({get_until, Prompt, M, F, As}, State) ->
    {ok, convert(State#state.nxt, latin1, State#state.mode), 
     State#state{nxt = eof, 
		 q = [{get_until, Prompt, M, F, As} | State#state.q]}};
request({get_chars, Prompt, N}, State) ->
    {ok, convert(State#state.nxt, latin1, State#state.mode), 
     State#state{nxt = eof, 
		 q = [{get_chars, Prompt, N} | 
		      State#state.q]}};
request({get_line, Prompt}, State) ->
    {ok, convert(State#state.nxt, latin1, State#state.mode), 
     State#state{nxt = eof, 
		 q = [{get_line, Prompt} | 
		      State#state.q]}};
request({get_geomentry,_}, State) ->
    {error, {error,enotsup}, State};
request({setopts, Opts}, State) when Opts =:= [{binary, false}]; Opts =:= [list] ->
    {ok, ok, State#state{q=[{setopts, Opts} | State#state.q ], mode = list}};
request({setopts, Opts}, State) when Opts =:= [{binary, true}]; Opts =:= [binary] ->
    {ok, ok, State#state{q=[{setopts, Opts} | State#state.q ], mode = binary}};
request(getopts, State) ->
    {ok, case State#state.mode of
	     list -> [{binary,false}];
	     binary -> [{binary, true}]
	 end, State#state{q=[getopts | State#state.q ]}};
request({requests, Reqs}, State) ->
    multi_request(Reqs, {ok, ok, State}).

multi_request([R|Rs], {ok, _Res, State}) ->
    multi_request(Rs, request(R, State));
multi_request([_|_], Error) ->
    Error;
multi_request([], State) ->
    State.

convert(Atom,_,_) when is_atom(Atom) ->
    Atom;
convert(Data, unicode, list) ->
    unicode:characters_to_list(Data,unicode);
convert(Data, latin1, list) ->
    try
	L = unicode:characters_to_list(Data, unicode),
	[ true = Ch =< 255 || Ch <- L ],
	L
    catch
	_:_ ->
	    {error, {cannot_convert, unicode, latin1}}
    end;
convert(Data, unicode, binary) ->
    unicode:characters_to_binary(Data,unicode,unicode);
convert(Data, latin1, binary) ->
    case unicode:characters_to_binary(Data, unicode, latin1) of
	Bin when is_binary(Bin) ->
	    Bin;
	_ ->
	    {error, {cannot_convert, unicode, latin1}}
    end.

atom2list(A) ->
    lists:flatten(io_lib:format("~w", [A])).

chomp([]) ->
    [];
chomp([$\n]) ->
    [];
chomp([H|T]) ->
    [H|chomp(T)];
chomp(<<>>) ->
    <<>>;
chomp(<<$\n>>) ->
    <<>>;
chomp(<<Ch,Rest/binary>>) ->
    X = chomp(Rest),
    <<Ch,X/binary>>;
chomp(Atom) ->
    Atom.

do(Fun) ->
    {_,Ref} = spawn_monitor(fun() ->
				    exit(Fun())
			    end),
    Ref.

done(Ref) ->
    receive
	{'DOWN',Ref,process,_,Result} ->
	    Result
    end.
