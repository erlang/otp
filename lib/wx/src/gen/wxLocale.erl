%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxLocale).
-moduledoc """
`m:wxLocale` class encapsulates all language-dependent settings and is a generalization
of the C locale concept.

In wxWidgets this class manages current locale. It also initializes and activates `wxTranslations`
(not implemented in wx) object that manages message catalogs.

For a list of the supported languages, please see ?wxLanguage enum values. These
constants may be used to specify the language in `init/3` and are returned by `getSystemLanguage/0`.

See:
* [Overview i18n](https://docs.wxwidgets.org/3.2/overview_i18n.html#overview_i18n)

* [Examples](https://docs.wxwidgets.org/3.2/page_samples.html#page_samples_internat)

wxWidgets docs: [wxLocale](https://docs.wxwidgets.org/3.2/classwx_locale.html)
""".
-include("wxe.hrl").
-export([addCatalog/2,addCatalog/3,addCatalog/4,addCatalogLookupPathPrefix/1,
  destroy/1,getCanonicalName/1,getHeaderValue/2,getHeaderValue/3,getLanguage/1,
  getLanguageName/1,getLocale/1,getName/1,getString/2,getString/3,getString/4,
  getString/5,getSysName/1,getSystemEncoding/0,getSystemEncodingName/0,
  getSystemLanguage/0,init/1,init/2,init/3,isLoaded/2,isOk/1,new/0,new/1,
  new/2]).

%% inherited exports
-export([parent_class/1]).

-type wxLocale() :: wx:wx_object().
-export_type([wxLocale/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
This is the default constructor and it does nothing to initialize the object: `init/3`
must be used to do that.
""".
-spec new() -> wxLocale().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxLocale_new_0),
  wxe_util:rec(?wxLocale_new_0).

-doc "Equivalent to: `new/2`".
-spec new(Language) -> wxLocale() when
	Language::integer();
      (Name) -> wxLocale() when
	Name::unicode:chardata().

new(Language)
 when is_integer(Language) ->
  new(Language, []);

new(Name)
 when ?is_chardata(Name) ->
  new(Name, []).

-doc """
See `init/3` for parameters description.

The call of this function has several global side effects which you should understand:
first of all, the application locale is changed - note that this will affect many of
standard C library functions such as printf() or strftime(). Second, this `m:wxLocale`
object becomes the new current global locale for the application and so all subsequent
calls to ?wxGetTranslation() will try to translate the messages using the message catalogs
for this locale.
""".
-spec new(Language, [Option]) -> wxLocale() when
	Language::integer(),
	Option :: {'flags', integer()};
      (Name, [Option]) -> wxLocale() when
	Name::unicode:chardata(),
	Option :: {'shortName', unicode:chardata()}
		 | {'locale', unicode:chardata()}
		 | {'bLoadDefault', boolean()}.
new(Language, Options)
 when is_integer(Language),is_list(Options) ->
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Language, Opts,?get_env(),?wxLocale_new_2_0),
  wxe_util:rec(?wxLocale_new_2_0);
new(Name, Options)
 when ?is_chardata(Name),is_list(Options) ->
  Name_UC = unicode:characters_to_binary(Name),
  MOpts = fun({shortName, ShortName}) ->   ShortName_UC = unicode:characters_to_binary(ShortName),{shortName,ShortName_UC};
          ({locale, Locale}) ->   Locale_UC = unicode:characters_to_binary(Locale),{locale,Locale_UC};
          ({bLoadDefault, _bLoadDefault} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Name_UC, Opts,?get_env(),?wxLocale_new_2_1),
  wxe_util:rec(?wxLocale_new_2_1).

-doc(#{equiv => init(This, [])}).
-spec init(This) -> boolean() when
	This::wxLocale().

init(This)
 when is_record(This, wx_ref) ->
  init(This, []).

-doc """
Initializes the `m:wxLocale` instance.

The call of this function has several global side effects which you should understand:
first of all, the application locale is changed - note that this will affect many of
standard C library functions such as printf() or strftime(). Second, this `m:wxLocale`
object becomes the new current global locale for the application and so all subsequent
calls to ?wxGetTranslation() will try to translate the messages using the message catalogs
for this locale.

Return: true on success or false if the given locale couldn't be set.
""".
-spec init(This, [Option]) -> boolean() when
	This::wxLocale(),
	Option :: {'language', integer()}
		 | {'flags', integer()}.
init(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLocale),
  MOpts = fun({language, _language} = Arg) -> Arg;
          ({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxLocale_Init_1),
  wxe_util:rec(?wxLocale_Init_1).

-doc """
Deprecated:

This form is deprecated, use the other one unless you know what you are doing.
""".
-spec init(This, Name, [Option]) -> boolean() when
	This::wxLocale(), Name::unicode:chardata(),
	Option :: {'shortName', unicode:chardata()}
		 | {'locale', unicode:chardata()}
		 | {'bLoadDefault', boolean()}.
init(#wx_ref{type=ThisT}=This,Name, Options)
 when ?is_chardata(Name),is_list(Options) ->
  ?CLASS(ThisT,wxLocale),
  Name_UC = unicode:characters_to_binary(Name),
  MOpts = fun({shortName, ShortName}) ->   ShortName_UC = unicode:characters_to_binary(ShortName),{shortName,ShortName_UC};
          ({locale, Locale}) ->   Locale_UC = unicode:characters_to_binary(Locale),{locale,Locale_UC};
          ({bLoadDefault, _bLoadDefault} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Name_UC, Opts,?get_env(),?wxLocale_Init_2),
  wxe_util:rec(?wxLocale_Init_2).

-doc "Calls wxTranslations::AddCatalog(const wxString&).".
-spec addCatalog(This, Domain) -> boolean() when
	This::wxLocale(), Domain::unicode:chardata().
addCatalog(#wx_ref{type=ThisT}=This,Domain)
 when ?is_chardata(Domain) ->
  ?CLASS(ThisT,wxLocale),
  Domain_UC = unicode:characters_to_binary(Domain),
  wxe_util:queue_cmd(This,Domain_UC,?get_env(),?wxLocale_AddCatalog_1),
  wxe_util:rec(?wxLocale_AddCatalog_1).

-doc "Calls `wxTranslations::AddCatalog(const wxString&, wxLanguage)` (not implemented in wx).".
%%  MsgIdLanguage = integer
-spec addCatalog(This, Domain, MsgIdLanguage) -> boolean() when
	This::wxLocale(), Domain::unicode:chardata(), MsgIdLanguage::wx:wx_enum().
addCatalog(#wx_ref{type=ThisT}=This,Domain,MsgIdLanguage)
 when ?is_chardata(Domain),is_integer(MsgIdLanguage) ->
  ?CLASS(ThisT,wxLocale),
  Domain_UC = unicode:characters_to_binary(Domain),
  wxe_util:queue_cmd(This,Domain_UC,MsgIdLanguage,?get_env(),?wxLocale_AddCatalog_2),
  wxe_util:rec(?wxLocale_AddCatalog_2).

-doc """
Calls `wxTranslations::AddCatalog(const wxString&, wxLanguage, const wxString&)` (not
implemented in wx).
""".
%%  MsgIdLanguage = integer
-spec addCatalog(This, Domain, MsgIdLanguage, MsgIdCharset) -> boolean() when
	This::wxLocale(), Domain::unicode:chardata(), MsgIdLanguage::wx:wx_enum(), MsgIdCharset::unicode:chardata().
addCatalog(#wx_ref{type=ThisT}=This,Domain,MsgIdLanguage,MsgIdCharset)
 when ?is_chardata(Domain),is_integer(MsgIdLanguage),?is_chardata(MsgIdCharset) ->
  ?CLASS(ThisT,wxLocale),
  Domain_UC = unicode:characters_to_binary(Domain),
  MsgIdCharset_UC = unicode:characters_to_binary(MsgIdCharset),
  wxe_util:queue_cmd(This,Domain_UC,MsgIdLanguage,MsgIdCharset_UC,?get_env(),?wxLocale_AddCatalog_3),
  wxe_util:rec(?wxLocale_AddCatalog_3).

-doc "Calls `wxFileTranslationsLoader::AddCatalogLookupPathPrefix()` (not implemented in wx).".
-spec addCatalogLookupPathPrefix(Prefix) -> 'ok' when
	Prefix::unicode:chardata().
addCatalogLookupPathPrefix(Prefix)
 when ?is_chardata(Prefix) ->
  Prefix_UC = unicode:characters_to_binary(Prefix),
  wxe_util:queue_cmd(Prefix_UC,?get_env(),?wxLocale_AddCatalogLookupPathPrefix).

-doc """
Returns the canonical form of current locale name.

Canonical form is the one that is used on UNIX systems: it is a two- or five-letter
string in xx or xx_YY format, where xx is ISO 639 code of language and YY is ISO 3166 code
of the country. Examples are "en", "en_GB", "en_US" or "fr_FR". This form is internally
used when looking up message catalogs. Compare `getSysName/1`.
""".
-spec getCanonicalName(This) -> unicode:charlist() when
	This::wxLocale().
getCanonicalName(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxLocale),
  wxe_util:queue_cmd(This,?get_env(),?wxLocale_GetCanonicalName),
  wxe_util:rec(?wxLocale_GetCanonicalName).

-doc """
Returns the ?wxLanguage constant of current language.

Note that you can call this function only if you used the form of `init/3` that takes ?wxLanguage
argument.
""".
-spec getLanguage(This) -> integer() when
	This::wxLocale().
getLanguage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxLocale),
  wxe_util:queue_cmd(This,?get_env(),?wxLocale_GetLanguage),
  wxe_util:rec(?wxLocale_GetLanguage).

-doc """
Returns English name of the given language or empty string if this language is unknown.

See `GetLanguageInfo()` (not implemented in wx) for a remark about special meaning of `wxLANGUAGE_DEFAULT`.
""".
-spec getLanguageName(Lang) -> unicode:charlist() when
	Lang::integer().
getLanguageName(Lang)
 when is_integer(Lang) ->
  wxe_util:queue_cmd(Lang,?get_env(),?wxLocale_GetLanguageName),
  wxe_util:rec(?wxLocale_GetLanguageName).

-doc """
Returns the locale name as passed to the constructor or `init/3`.

This is a full, human-readable name, e.g. "English" or "French".
""".
-spec getLocale(This) -> unicode:charlist() when
	This::wxLocale().
getLocale(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxLocale),
  wxe_util:queue_cmd(This,?get_env(),?wxLocale_GetLocale),
  wxe_util:rec(?wxLocale_GetLocale).

-doc """
Returns the current short name for the locale (as given to the constructor or the `init/3`
function).
""".
-spec getName(This) -> unicode:charlist() when
	This::wxLocale().
getName(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxLocale),
  wxe_util:queue_cmd(This,?get_env(),?wxLocale_GetName),
  wxe_util:rec(?wxLocale_GetName).

-doc(#{equiv => getString(This,OrigString, [])}).
-spec getString(This, OrigString) -> unicode:charlist() when
	This::wxLocale(), OrigString::unicode:chardata().

getString(This,OrigString)
 when is_record(This, wx_ref),?is_chardata(OrigString) ->
  getString(This,OrigString, []).

-doc "Calls wxGetTranslation(const wxString&, const wxString&).".
-spec getString(This, OrigString, [Option]) -> unicode:charlist() when
	This::wxLocale(), OrigString::unicode:chardata(),
	Option :: {'szDomain', unicode:chardata()}.
getString(#wx_ref{type=ThisT}=This,OrigString, Options)
 when ?is_chardata(OrigString),is_list(Options) ->
  ?CLASS(ThisT,wxLocale),
  OrigString_UC = unicode:characters_to_binary(OrigString),
  MOpts = fun({szDomain, SzDomain}) ->   SzDomain_UC = unicode:characters_to_binary(SzDomain),{szDomain,SzDomain_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,OrigString_UC, Opts,?get_env(),?wxLocale_GetString_2),
  wxe_util:rec(?wxLocale_GetString_2).

-doc(#{equiv => getString(This,OrigString,OrigString2,N, [])}).
-spec getString(This, OrigString, OrigString2, N) -> unicode:charlist() when
	This::wxLocale(), OrigString::unicode:chardata(), OrigString2::unicode:chardata(), N::integer().

getString(This,OrigString,OrigString2,N)
 when is_record(This, wx_ref),?is_chardata(OrigString),?is_chardata(OrigString2),is_integer(N) ->
  getString(This,OrigString,OrigString2,N, []).

-doc "Calls wxGetTranslation(const wxString&, const wxString&, unsigned, const wxString&).".
-spec getString(This, OrigString, OrigString2, N, [Option]) -> unicode:charlist() when
	This::wxLocale(), OrigString::unicode:chardata(), OrigString2::unicode:chardata(), N::integer(),
	Option :: {'szDomain', unicode:chardata()}.
getString(#wx_ref{type=ThisT}=This,OrigString,OrigString2,N, Options)
 when ?is_chardata(OrigString),?is_chardata(OrigString2),is_integer(N),is_list(Options) ->
  ?CLASS(ThisT,wxLocale),
  OrigString_UC = unicode:characters_to_binary(OrigString),
  OrigString2_UC = unicode:characters_to_binary(OrigString2),
  MOpts = fun({szDomain, SzDomain}) ->   SzDomain_UC = unicode:characters_to_binary(SzDomain),{szDomain,SzDomain_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,OrigString_UC,OrigString2_UC,N, Opts,?get_env(),?wxLocale_GetString_4),
  wxe_util:rec(?wxLocale_GetString_4).

-doc(#{equiv => getHeaderValue(This,Header, [])}).
-spec getHeaderValue(This, Header) -> unicode:charlist() when
	This::wxLocale(), Header::unicode:chardata().

getHeaderValue(This,Header)
 when is_record(This, wx_ref),?is_chardata(Header) ->
  getHeaderValue(This,Header, []).

-doc "Calls `wxTranslations::GetHeaderValue()` (not implemented in wx).".
-spec getHeaderValue(This, Header, [Option]) -> unicode:charlist() when
	This::wxLocale(), Header::unicode:chardata(),
	Option :: {'szDomain', unicode:chardata()}.
getHeaderValue(#wx_ref{type=ThisT}=This,Header, Options)
 when ?is_chardata(Header),is_list(Options) ->
  ?CLASS(ThisT,wxLocale),
  Header_UC = unicode:characters_to_binary(Header),
  MOpts = fun({szDomain, SzDomain}) ->   SzDomain_UC = unicode:characters_to_binary(SzDomain),{szDomain,SzDomain_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Header_UC, Opts,?get_env(),?wxLocale_GetHeaderValue),
  wxe_util:rec(?wxLocale_GetHeaderValue).

-doc """
Returns current platform-specific locale name as passed to setlocale().

Compare `getCanonicalName/1`.
""".
-spec getSysName(This) -> unicode:charlist() when
	This::wxLocale().
getSysName(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxLocale),
  wxe_util:queue_cmd(This,?get_env(),?wxLocale_GetSysName),
  wxe_util:rec(?wxLocale_GetSysName).

-doc """
Tries to detect the user's default font encoding.

Returns ?wxFontEncoding() value or `wxFONTENCODING_SYSTEM` if it couldn't be determined.
""".
%%  Res = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
-spec getSystemEncoding() -> wx:wx_enum().
getSystemEncoding() ->
  wxe_util:queue_cmd(?get_env(), ?wxLocale_GetSystemEncoding),
  wxe_util:rec(?wxLocale_GetSystemEncoding).

-doc """
Tries to detect the name of the user's default font encoding.

This string isn't particularly useful for the application as its form is
platform-dependent and so you should probably use `getSystemEncoding/0` instead.

Returns a user-readable string value or an empty string if it couldn't be determined.
""".
-spec getSystemEncodingName() -> unicode:charlist().
getSystemEncodingName() ->
  wxe_util:queue_cmd(?get_env(), ?wxLocale_GetSystemEncodingName),
  wxe_util:rec(?wxLocale_GetSystemEncodingName).

-doc """
Tries to detect the user's default locale setting.

Returns the ?wxLanguage value or `wxLANGUAGE_UNKNOWN` if the language-guessing algorithm failed.

Note: This function works with `locales` and returns the user's default locale. This may
be, and usually is, the same as their preferred UI language, but it's not the same thing.
Use wxTranslation to obtain `language` information.
""".
-spec getSystemLanguage() -> integer().
getSystemLanguage() ->
  wxe_util:queue_cmd(?get_env(), ?wxLocale_GetSystemLanguage),
  wxe_util:rec(?wxLocale_GetSystemLanguage).

-doc "Calls `wxTranslations::IsLoaded()` (not implemented in wx).".
-spec isLoaded(This, Domain) -> boolean() when
	This::wxLocale(), Domain::unicode:chardata().
isLoaded(#wx_ref{type=ThisT}=This,Domain)
 when ?is_chardata(Domain) ->
  ?CLASS(ThisT,wxLocale),
  Domain_UC = unicode:characters_to_binary(Domain),
  wxe_util:queue_cmd(This,Domain_UC,?get_env(),?wxLocale_IsLoaded),
  wxe_util:rec(?wxLocale_IsLoaded).

-doc "Returns true if the locale could be set successfully.".
-spec isOk(This) -> boolean() when
	This::wxLocale().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxLocale),
  wxe_util:queue_cmd(This,?get_env(),?wxLocale_IsOk),
  wxe_util:rec(?wxLocale_IsOk).

-doc "Destroys the object".
-spec destroy(This::wxLocale()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxLocale),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxLocale_destruct),
  ok.
