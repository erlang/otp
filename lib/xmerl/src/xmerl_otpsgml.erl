%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

%%% Description  : Callback module for exporting XHTML to OTP-SGML.

-module(xmerl_otpsgml).

-export(['#xml-inheritance#'/0]).

%% Note: we assume XML data, so all tags are lowercase!

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1,
	 p/4]).

-import(xmerl_lib, [markup/3, start_tag/2, is_empty_data/1,
		    export_text/1]).

-include("xmerl.hrl").
-include("xmerl_internal.hrl").


'#xml-inheritance#'() -> [xmerl_sgml].


%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
    export_text(Text).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, _Attrs, [], _E) -> 
    ["<!doctype erlref PUBLIC \"-//Stork//DTD erlref//EN\">\n",Data].


%% Note that SGML does not have the <Tag/> empty-element form.
%% Furthermore, for some element types, the end tag is forbidden. (In
%% all other cases, we always generate the end tag, to make sure that
%% the scope of a markup is not extended by mistake.)

'#element#'(Tag, Data, Attrs, _Parents, _E) ->
%    ?dbg("parents:\n~p\n",[_Parents]),
    case convert_tag(Tag,Attrs) of
	{false,NewTag,NewAttrs} ->
	    markup(NewTag, NewAttrs, Data);
	{true,NewTag,NewAttrs} ->
	    [start_tag(NewTag, NewAttrs), Data]
    end.


%% HTML tags with special handling

p(Data, Attrs, _Parents, _E) ->
    %% In general, we cannot drop the end tag for paragraph elements;
    %% that is only allowed if we know that it is immediately followed
    %% by some other block-level tag.
    case is_empty_data(Data) of
	true ->
	    %% Paragraph elements should never be completely empty.
	    markup(p, Attrs, "\s");
	false ->
	    markup(p, Attrs, Data)
    end.


%% Utility functions

convert_tag(code,Attrs) -> convert_tag(c,Attrs);
convert_tag(strong,Attrs) -> convert_tag(em,Attrs);
convert_tag(b,Attrs) -> convert_tag(em,Attrs);
convert_tag(underline,Attrs) -> convert_tag(em,Attrs); % what is underline in sgml???
convert_tag(dl,Attrs) -> convert_tag(taglist,Attrs);
convert_tag(dt,Attrs) -> convert_tag(tag,Attrs);
convert_tag(dd,Attrs) -> convert_tag(item,Attrs);
convert_tag(ul,Attrs) -> convert_tag(list,Attrs);
convert_tag(li,Attrs) -> convert_tag(item,Attrs);
convert_tag(tt,Attrs) -> convert_tag(c,Attrs);
%convert_tag(a, Attrs) -> convert_tag(seealso,convert_seealso_attrs(Attrs));
convert_tag(a, Attrs) -> convert_tag(convert_aref(Attrs),convert_aref_attrs(convert_aref(Attrs),Attrs));
convert_tag(Tag,Attrs) -> {forbid_end(Tag),Tag,Attrs}.

convert_aref([#xmlAttribute{name = href, value = V}|_Rest]) ->
    %% search if it is a html link, thus make it a 'url' ref otherwise
    %% a 'seealso'.
    case html_content(V) of
	true ->
	    url;
	_ ->
	    seealso
    end;
convert_aref([#xmlAttribute{name = K}|Rest]) ->
    error_logger:warning_msg("ignoring attribute \'~p\' for tag \'a\'\n",[K]),
    convert_aref(Rest).
convert_aref_attrs(url,Attrs) ->
    Attrs;
convert_aref_attrs(SA,[#xmlAttribute{name = href, value = V}=A|Rest]) ->
    [A#xmlAttribute{name=marker,value=V}|convert_aref_attrs(SA,Rest)];
convert_aref_attrs(_,[])->
    [].
html_content([]) ->
    false;
html_content([$.|Rest]) ->
    case Rest of
	"htm"++_EmaNfeR ->
	    true;
	_ -> html_content(Rest)
    end;
html_content([_H|T]) ->
    html_content(T).

% convert_seealso_attrs([#xmlAttribute{name = href, value = V} = A|Rest]) ->
%     [A#xmlAttribute{name=marker,value=normalize_web_ref(V)}|convert_seealso_attrs(Rest)];
% convert_seealso_attrs([#xmlAttribute{name = K}|Rest]) ->
%     error_logger:warning_msg("ignoring attribute \'~p\' for tag \'a\'\n",[K]),
%     convert_seealso_attrs(Rest);
% convert_seealso_attrs([]) ->
%     [].

% normalize_web_ref(RefName) ->
%     normalize_web_ref1(lists:reverse(RefName)).

% normalize_web_ref1("lmth."++EmaNfeR) ->
%     lists:reverse(EmaNfeR);
% normalize_web_ref1("mth"++EmaNfeR) ->
%     lists:reverse(EmaNfeR);
% normalize_web_ref1(RefName) ->
%     RefName.

forbid_end(area) -> true; 
forbid_end(base) -> true; 
forbid_end(basefont) -> true; 
forbid_end(br) -> true; 
forbid_end(col) -> true; 
forbid_end(frame) -> true; 
forbid_end(hr) -> true; 
forbid_end(img) -> true; 
forbid_end(input) -> true; 
forbid_end(isindex) -> true; 
forbid_end(link) -> true; 
forbid_end(marker) -> true; 
forbid_end(meta) -> true; 
forbid_end(param) -> true; 
forbid_end(_) -> false.
