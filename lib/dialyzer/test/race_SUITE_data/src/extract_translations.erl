%%%----------------------------------------------------------------------
%%% File    : extract_translations.erl
%%% Author  : Sergei Golovan <sgolovan@nes.ru>
%%% Purpose : Auxiliary tool for interface/messages translators
%%% Created : 23 Apr 2005 by Sergei Golovan <sgolovan@nes.ru>
%%% Id      : $Id: extract_translations.erl,v 1.1 2009/08/17 09:18:59 maria Exp $
%%%----------------------------------------------------------------------

-module(extract_translations).
-author('sgolovan@nes.ru').

-export([start/0]).

-define(STATUS_SUCCESS, 0).
-define(STATUS_ERROR,   1).
-define(STATUS_USAGE,   2).

-include_lib("kernel/include/file.hrl").


start() ->
    ets:new(translations, [named_table, public]),
    ets:new(translations_obsolete, [named_table, public]),
    ets:new(files, [named_table, public]),
    ets:new(vars, [named_table, public]),
    case init:get_plain_arguments() of
	["-srcmsg2po", Dir, File] ->
	    print_po_header(File),
	    Status = process(Dir, File, srcmsg2po),
	    halt(Status);
	["-unused", Dir, File] ->
	    Status = process(Dir, File, unused),
	    halt(Status);
	[Dir, File] ->
	    Status = process(Dir, File, used),
	    halt(Status);
	_ ->
	    print_usage(),
	    halt(?STATUS_USAGE)
    end.


process(Dir, File, Used) ->
    case load_file(File) of
	{error, Reason} ->
	    io:format("~s: ~s~n", [File, file:format_error(Reason)]),
	    ?STATUS_ERROR;
	_ ->
	    FileList = find_src_files(Dir),
	    lists:foreach(
	      fun(F) ->
		    parse_file(Dir, F, Used)
	      end, FileList),
	    case Used of
		unused ->
		    ets:foldl(fun({Key, _}, _) ->
				      io:format("~p~n", [Key])
			      end, ok, translations);
		srcmsg2po ->
		    ets:foldl(fun({Key, Trans}, _) ->
				      print_translation_obsolete(Key, Trans)
			      end, ok, translations_obsolete);
		_ ->
		    ok
	    end,
	    ?STATUS_SUCCESS
    end.

parse_file(Dir, File, Used) ->
    ets:delete_all_objects(vars),
    case epp:parse_file(File, [Dir, filename:dirname(File) | code:get_path()], []) of
	{ok, Forms} ->
	    lists:foreach(
	      fun(F) ->
		    parse_form(Dir, File, F, Used)
	      end, Forms);
	_ ->
	    ok
    end.

parse_form(Dir, File, Form, Used) ->
    case Form of
	%%{undefined, Something} ->
        %% io:format("Undefined: ~p~n", [Something]);
	{call,
	 _,
	 {remote, _, {atom, _, translate}, {atom, _, translate}},
	 [_, {string, Line, Str}]
	} ->
	    process_string(Dir, File, Line, Str, Used);
	{call,
	 _,
	 {remote, _, {atom, _, translate}, {atom, _, translate}},
	 [_, {var, _, Name}]
	} ->
	    case ets:lookup(vars, Name) of
		[{_Name, Value, Line}] ->
		    process_string(Dir, File, Line, Value, Used);
		_ ->
		    ok
	    end;
	{match,
	 _,
	 {var, _, Name},
	 {string, Line, Value}
	} ->
	    ets:insert(vars, {Name, Value, Line});
	L when is_list(L) ->
	    lists:foreach(
	      fun(F) ->
		      parse_form(Dir, File, F, Used)
	      end, L);
	T when is_tuple(T) ->
	    lists:foreach(
	      fun(F) ->
		      parse_form(Dir, File, F, Used)
	      end, tuple_to_list(T));
	_ ->
	    ok
    end.

process_string(_Dir, _File, _Line, "", _Used) ->
    ok;

process_string(_Dir, File, Line, Str, Used) ->
    case {ets:lookup(translations, Str), Used} of
	{[{_Key, _Trans}], unused} ->
	    ets:delete(translations, Str);
	{[{_Key, _Trans}], used} ->
	    ok;
	{[{_Key, Trans}], srcmsg2po} ->
	    ets:delete(translations_obsolete, Str),
	    print_translation(File, Line, Str, Trans);
	{_, used} ->
	    case ets:lookup(files, File) of
		[{_}] ->
		    ok;
		_ ->
		    io:format("~n% ~s~n", [File]),
		    ets:insert(files, {File})
	    end,
	    case Str of
		[] -> ok;
		_ -> io:format("{~p, \"\"}.~n", [Str])
	    end,
	    ets:insert(translations, {Str, ""});
	{_, srcmsg2po} ->
	    case ets:lookup(files, File) of
		[{_}] ->
		    ok;
		_ ->
		    ets:insert(files, {File})
	    end,
	    ets:insert(translations, {Str, ""}),
	    print_translation(File, Line, Str, "");
	_ ->
	    ok
    end.

load_file(File) ->
    case file:consult(File) of
	{ok, Terms} ->
	    lists:foreach(
	      fun({Orig, Trans}) ->
		    case Trans of
			"" ->
			    ok;
			_ ->
			    ets:insert(translations, {Orig, Trans}),
			    ets:insert(translations_obsolete, {Orig, Trans})
		    end
	      end, Terms);
	Err ->
	    Err
    end.

find_src_files(Dir) ->
    case file:list_dir(Dir) of
	{ok, FileList} ->
	    recurse_filelist(
	      lists:map(
	        fun(F) ->
			filename:join(Dir, F)
		end, FileList));
	_ ->
	    []
    end.

recurse_filelist(FileList) ->
    recurse_filelist(FileList, []).

recurse_filelist([], Acc) ->
    lists:reverse(Acc);

recurse_filelist([H | T], Acc) ->
    case file:read_file_info(H) of
	{ok, #file_info{type = directory}} ->
	    recurse_filelist(T, lists:reverse(find_src_files(H)) ++ Acc);
	{ok, #file_info{type = regular}} ->
	    case string:substr(H, string:len(H) - 3) of
		".erl" ->
		    recurse_filelist(T, [H | Acc]);
		".hrl" ->
		    recurse_filelist(T, [H | Acc]);
		_ ->
		    recurse_filelist(T, Acc)
	    end;
	_ ->
	    recurse_filelist(T, Acc)
    end.


print_usage() ->
    io:format(
      "Usage: extract_translations [-unused] dir file~n"
      "~n"
      "Example:~n"
      "  extract_translations . ./msgs/ru.msg~n"
     ).


%%%
%%% Gettext
%%%

print_po_header(File) ->
    MsgProps = get_msg_header_props(File),
    {Language, [LastT | AddT]} = prepare_props(MsgProps),
    application:load(ejabberd),
    {ok, Version} = application:get_key(ejabberd, vsn),
    print_po_header(Version, Language, LastT, AddT).

get_msg_header_props(File) ->
    {ok, F} = file:open(File, [read]),
    Lines = get_msg_header_props(F, []),
    file:close(F),
    Lines.

get_msg_header_props(F, Lines) ->
    String = io:get_line(F, ""),
    case io_lib:fread("% ", String) of
	{ok, [], RemString} ->
	    case io_lib:fread("~s", RemString) of
		{ok, [Key], Value} when Value /= "\n" ->
		    %% The first character in Value is a blankspace:
		    %% And the last characters are 'slash n'
		    ValueClean = string:substr(Value, 2, string:len(Value)-2),
		    get_msg_header_props(F, Lines ++ [{Key, ValueClean}]);
		_ ->
		    get_msg_header_props(F, Lines)
	    end;
	_ ->
	    Lines
    end.

prepare_props(MsgProps) ->
    Language = proplists:get_value("Language:", MsgProps),
    Authors = proplists:get_all_values("Author:", MsgProps),
    {Language, Authors}.

print_po_header(Version, Language, LastTranslator, AdditionalTranslatorsList) ->
    AdditionalTranslatorsString = build_additional_translators(AdditionalTranslatorsList),
    HeaderString =
	"msgid \"\"\n"
	"msgstr \"\"\n"
	"\"Project-Id-Version: " ++ Version ++ "\\n\"\n"
	++ "\"X-Language: " ++ Language ++ "\\n\"\n"
	"\"Last-Translator: " ++ LastTranslator ++ "\\n\"\n"
	++ AdditionalTranslatorsString ++
	"\"MIME-Version: 1.0\\n\"\n"
	"\"Content-Type: text/plain; charset=UTF-8\\n\"\n"
	"\"Content-Transfer-Encoding: 8bit\\n\"\n",
    io:format("~s~n", [HeaderString]).

build_additional_translators(List) ->
    lists:foldl(
      fun(T, Str) ->
	      Str ++ "\"X-Additional-Translator: " ++ T ++ "\\n\"\n"
      end,
      "",
      List).

print_translation(File, Line, Str, StrT) ->
    {ok, StrQ, _} = regexp:gsub(Str, "\"", "\\\""),
    {ok, StrTQ, _} = regexp:gsub(StrT, "\"", "\\\""),
    io:format("#: ~s:~p~nmsgid \"~s\"~nmsgstr \"~s\"~n~n", [File, Line, StrQ, StrTQ]).

print_translation_obsolete(Str, StrT) ->
    File = "unknown.erl",
    Line = 1,
    {ok, StrQ, _} = regexp:gsub(Str, "\"", "\\\""),
    {ok, StrTQ, _} = regexp:gsub(StrT, "\"", "\\\""),
    io:format("#: ~s:~p~n#~~ msgid \"~s\"~n#~~ msgstr \"~s\"~n~n", [File, Line, StrQ, StrTQ]).
