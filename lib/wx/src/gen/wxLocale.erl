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
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html">wxLocale</a>.
%% @type wxLocale().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxLocale).
-include("wxe.hrl").
-export([addCatalog/2,addCatalog/4,addCatalogLookupPathPrefix/1,destroy/1,
  getCanonicalName/1,getHeaderValue/2,getHeaderValue/3,getLanguage/1,
  getLanguageName/1,getLocale/1,getName/1,getString/2,getString/3,getString/4,
  getString/5,getSysName/1,getSystemEncoding/0,getSystemEncodingName/0,
  getSystemLanguage/0,init/1,init/2,isLoaded/2,isOk/1,new/0,new/1,new/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxLocale/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxLocale() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalewxlocale">external documentation</a>.
-spec new() -> wxLocale().
new() ->
  wxe_util:construct(?wxLocale_new_0,
  <<>>).

%% @equiv new(Language, [])
-spec new(Language) -> wxLocale() when
	Language::integer().

new(Language)
 when is_integer(Language) ->
  new(Language, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalewxlocale">external documentation</a>.
-spec new(Language, [Option]) -> wxLocale() when
	Language::integer(),
	Option :: {'flags', integer()}.
new(Language, Options)
 when is_integer(Language),is_list(Options) ->
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxLocale_new_2,
  <<Language:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv init(This, [])
-spec init(This) -> boolean() when
	This::wxLocale().

init(This)
 when is_record(This, wx_ref) ->
  init(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocaleinit">external documentation</a>.
-spec init(This, [Option]) -> boolean() when
	This::wxLocale(),
	Option :: {'language', integer()}
		 | {'flags', integer()}.
init(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLocale),
  MOpts = fun({language, Language}, Acc) -> [<<1:32/?UI,Language:32/?UI>>|Acc];
          ({flags, Flags}, Acc) -> [<<2:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxLocale_Init,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocaleaddcatalog">external documentation</a>.
-spec addCatalog(This, SzDomain) -> boolean() when
	This::wxLocale(), SzDomain::unicode:chardata().
addCatalog(#wx_ref{type=ThisT,ref=ThisRef},SzDomain)
 when is_list(SzDomain) ->
  ?CLASS(ThisT,wxLocale),
  SzDomain_UC = unicode:characters_to_binary([SzDomain,0]),
  wxe_util:call(?wxLocale_AddCatalog_1,
  <<ThisRef:32/?UI,(byte_size(SzDomain_UC)):32/?UI,(SzDomain_UC)/binary, 0:(((8- ((0+byte_size(SzDomain_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocaleaddcatalog">external documentation</a>.
%%<br /> MsgIdLanguage = ?wxLANGUAGE_DEFAULT | ?wxLANGUAGE_UNKNOWN | ?wxLANGUAGE_ABKHAZIAN | ?wxLANGUAGE_AFAR | ?wxLANGUAGE_AFRIKAANS | ?wxLANGUAGE_ALBANIAN | ?wxLANGUAGE_AMHARIC | ?wxLANGUAGE_ARABIC | ?wxLANGUAGE_ARABIC_ALGERIA | ?wxLANGUAGE_ARABIC_BAHRAIN | ?wxLANGUAGE_ARABIC_EGYPT | ?wxLANGUAGE_ARABIC_IRAQ | ?wxLANGUAGE_ARABIC_JORDAN | ?wxLANGUAGE_ARABIC_KUWAIT | ?wxLANGUAGE_ARABIC_LEBANON | ?wxLANGUAGE_ARABIC_LIBYA | ?wxLANGUAGE_ARABIC_MOROCCO | ?wxLANGUAGE_ARABIC_OMAN | ?wxLANGUAGE_ARABIC_QATAR | ?wxLANGUAGE_ARABIC_SAUDI_ARABIA | ?wxLANGUAGE_ARABIC_SUDAN | ?wxLANGUAGE_ARABIC_SYRIA | ?wxLANGUAGE_ARABIC_TUNISIA | ?wxLANGUAGE_ARABIC_UAE | ?wxLANGUAGE_ARABIC_YEMEN | ?wxLANGUAGE_ARMENIAN | ?wxLANGUAGE_ASSAMESE | ?wxLANGUAGE_AYMARA | ?wxLANGUAGE_AZERI | ?wxLANGUAGE_AZERI_CYRILLIC | ?wxLANGUAGE_AZERI_LATIN | ?wxLANGUAGE_BASHKIR | ?wxLANGUAGE_BASQUE | ?wxLANGUAGE_BELARUSIAN | ?wxLANGUAGE_BENGALI | ?wxLANGUAGE_BHUTANI | ?wxLANGUAGE_BIHARI | ?wxLANGUAGE_BISLAMA | ?wxLANGUAGE_BRETON | ?wxLANGUAGE_BULGARIAN | ?wxLANGUAGE_BURMESE | ?wxLANGUAGE_CAMBODIAN | ?wxLANGUAGE_CATALAN | ?wxLANGUAGE_CHINESE | ?wxLANGUAGE_CHINESE_SIMPLIFIED | ?wxLANGUAGE_CHINESE_TRADITIONAL | ?wxLANGUAGE_CHINESE_HONGKONG | ?wxLANGUAGE_CHINESE_MACAU | ?wxLANGUAGE_CHINESE_SINGAPORE | ?wxLANGUAGE_CHINESE_TAIWAN | ?wxLANGUAGE_CORSICAN | ?wxLANGUAGE_CROATIAN | ?wxLANGUAGE_CZECH | ?wxLANGUAGE_DANISH | ?wxLANGUAGE_DUTCH | ?wxLANGUAGE_DUTCH_BELGIAN | ?wxLANGUAGE_ENGLISH | ?wxLANGUAGE_ENGLISH_UK | ?wxLANGUAGE_ENGLISH_US | ?wxLANGUAGE_ENGLISH_AUSTRALIA | ?wxLANGUAGE_ENGLISH_BELIZE | ?wxLANGUAGE_ENGLISH_BOTSWANA | ?wxLANGUAGE_ENGLISH_CANADA | ?wxLANGUAGE_ENGLISH_CARIBBEAN | ?wxLANGUAGE_ENGLISH_DENMARK | ?wxLANGUAGE_ENGLISH_EIRE | ?wxLANGUAGE_ENGLISH_JAMAICA | ?wxLANGUAGE_ENGLISH_NEW_ZEALAND | ?wxLANGUAGE_ENGLISH_PHILIPPINES | ?wxLANGUAGE_ENGLISH_SOUTH_AFRICA | ?wxLANGUAGE_ENGLISH_TRINIDAD | ?wxLANGUAGE_ENGLISH_ZIMBABWE | ?wxLANGUAGE_ESPERANTO | ?wxLANGUAGE_ESTONIAN | ?wxLANGUAGE_FAEROESE | ?wxLANGUAGE_FARSI | ?wxLANGUAGE_FIJI | ?wxLANGUAGE_FINNISH | ?wxLANGUAGE_FRENCH | ?wxLANGUAGE_FRENCH_BELGIAN | ?wxLANGUAGE_FRENCH_CANADIAN | ?wxLANGUAGE_FRENCH_LUXEMBOURG | ?wxLANGUAGE_FRENCH_MONACO | ?wxLANGUAGE_FRENCH_SWISS | ?wxLANGUAGE_FRISIAN | ?wxLANGUAGE_GALICIAN | ?wxLANGUAGE_GEORGIAN | ?wxLANGUAGE_GERMAN | ?wxLANGUAGE_GERMAN_AUSTRIAN | ?wxLANGUAGE_GERMAN_BELGIUM | ?wxLANGUAGE_GERMAN_LIECHTENSTEIN | ?wxLANGUAGE_GERMAN_LUXEMBOURG | ?wxLANGUAGE_GERMAN_SWISS | ?wxLANGUAGE_GREEK | ?wxLANGUAGE_GREENLANDIC | ?wxLANGUAGE_GUARANI | ?wxLANGUAGE_GUJARATI | ?wxLANGUAGE_HAUSA | ?wxLANGUAGE_HEBREW | ?wxLANGUAGE_HINDI | ?wxLANGUAGE_HUNGARIAN | ?wxLANGUAGE_ICELANDIC | ?wxLANGUAGE_INDONESIAN | ?wxLANGUAGE_INTERLINGUA | ?wxLANGUAGE_INTERLINGUE | ?wxLANGUAGE_INUKTITUT | ?wxLANGUAGE_INUPIAK | ?wxLANGUAGE_IRISH | ?wxLANGUAGE_ITALIAN | ?wxLANGUAGE_ITALIAN_SWISS | ?wxLANGUAGE_JAPANESE | ?wxLANGUAGE_JAVANESE | ?wxLANGUAGE_KANNADA | ?wxLANGUAGE_KASHMIRI | ?wxLANGUAGE_KASHMIRI_INDIA | ?wxLANGUAGE_KAZAKH | ?wxLANGUAGE_KERNEWEK | ?wxLANGUAGE_KINYARWANDA | ?wxLANGUAGE_KIRGHIZ | ?wxLANGUAGE_KIRUNDI | ?wxLANGUAGE_KONKANI | ?wxLANGUAGE_KOREAN | ?wxLANGUAGE_KURDISH | ?wxLANGUAGE_LAOTHIAN | ?wxLANGUAGE_LATIN | ?wxLANGUAGE_LATVIAN | ?wxLANGUAGE_LINGALA | ?wxLANGUAGE_LITHUANIAN | ?wxLANGUAGE_MACEDONIAN | ?wxLANGUAGE_MALAGASY | ?wxLANGUAGE_MALAY | ?wxLANGUAGE_MALAYALAM | ?wxLANGUAGE_MALAY_BRUNEI_DARUSSALAM | ?wxLANGUAGE_MALAY_MALAYSIA | ?wxLANGUAGE_MALTESE | ?wxLANGUAGE_MANIPURI | ?wxLANGUAGE_MAORI | ?wxLANGUAGE_MARATHI | ?wxLANGUAGE_MOLDAVIAN | ?wxLANGUAGE_MONGOLIAN | ?wxLANGUAGE_NAURU | ?wxLANGUAGE_NEPALI | ?wxLANGUAGE_NEPALI_INDIA | ?wxLANGUAGE_NORWEGIAN_BOKMAL | ?wxLANGUAGE_NORWEGIAN_NYNORSK | ?wxLANGUAGE_OCCITAN | ?wxLANGUAGE_ORIYA | ?wxLANGUAGE_OROMO | ?wxLANGUAGE_PASHTO | ?wxLANGUAGE_POLISH | ?wxLANGUAGE_PORTUGUESE | ?wxLANGUAGE_PORTUGUESE_BRAZILIAN | ?wxLANGUAGE_PUNJABI | ?wxLANGUAGE_QUECHUA | ?wxLANGUAGE_RHAETO_ROMANCE | ?wxLANGUAGE_ROMANIAN | ?wxLANGUAGE_RUSSIAN | ?wxLANGUAGE_RUSSIAN_UKRAINE | ?wxLANGUAGE_SAMOAN | ?wxLANGUAGE_SANGHO | ?wxLANGUAGE_SANSKRIT | ?wxLANGUAGE_SCOTS_GAELIC | ?wxLANGUAGE_SERBIAN | ?wxLANGUAGE_SERBIAN_CYRILLIC | ?wxLANGUAGE_SERBIAN_LATIN | ?wxLANGUAGE_SERBO_CROATIAN | ?wxLANGUAGE_SESOTHO | ?wxLANGUAGE_SETSWANA | ?wxLANGUAGE_SHONA | ?wxLANGUAGE_SINDHI | ?wxLANGUAGE_SINHALESE | ?wxLANGUAGE_SISWATI | ?wxLANGUAGE_SLOVAK | ?wxLANGUAGE_SLOVENIAN | ?wxLANGUAGE_SOMALI | ?wxLANGUAGE_SPANISH | ?wxLANGUAGE_SPANISH_ARGENTINA | ?wxLANGUAGE_SPANISH_BOLIVIA | ?wxLANGUAGE_SPANISH_CHILE | ?wxLANGUAGE_SPANISH_COLOMBIA | ?wxLANGUAGE_SPANISH_COSTA_RICA | ?wxLANGUAGE_SPANISH_DOMINICAN_REPUBLIC | ?wxLANGUAGE_SPANISH_ECUADOR | ?wxLANGUAGE_SPANISH_EL_SALVADOR | ?wxLANGUAGE_SPANISH_GUATEMALA | ?wxLANGUAGE_SPANISH_HONDURAS | ?wxLANGUAGE_SPANISH_MEXICAN | ?wxLANGUAGE_SPANISH_MODERN | ?wxLANGUAGE_SPANISH_NICARAGUA | ?wxLANGUAGE_SPANISH_PANAMA | ?wxLANGUAGE_SPANISH_PARAGUAY | ?wxLANGUAGE_SPANISH_PERU | ?wxLANGUAGE_SPANISH_PUERTO_RICO | ?wxLANGUAGE_SPANISH_URUGUAY | ?wxLANGUAGE_SPANISH_US | ?wxLANGUAGE_SPANISH_VENEZUELA | ?wxLANGUAGE_SUNDANESE | ?wxLANGUAGE_SWAHILI | ?wxLANGUAGE_SWEDISH | ?wxLANGUAGE_SWEDISH_FINLAND | ?wxLANGUAGE_TAGALOG | ?wxLANGUAGE_TAJIK | ?wxLANGUAGE_TAMIL | ?wxLANGUAGE_TATAR | ?wxLANGUAGE_TELUGU | ?wxLANGUAGE_THAI | ?wxLANGUAGE_TIBETAN | ?wxLANGUAGE_TIGRINYA | ?wxLANGUAGE_TONGA | ?wxLANGUAGE_TSONGA | ?wxLANGUAGE_TURKISH | ?wxLANGUAGE_TURKMEN | ?wxLANGUAGE_TWI | ?wxLANGUAGE_UIGHUR | ?wxLANGUAGE_UKRAINIAN | ?wxLANGUAGE_URDU | ?wxLANGUAGE_URDU_INDIA | ?wxLANGUAGE_URDU_PAKISTAN | ?wxLANGUAGE_UZBEK | ?wxLANGUAGE_UZBEK_CYRILLIC | ?wxLANGUAGE_UZBEK_LATIN | ?wxLANGUAGE_VIETNAMESE | ?wxLANGUAGE_VOLAPUK | ?wxLANGUAGE_WELSH | ?wxLANGUAGE_WOLOF | ?wxLANGUAGE_XHOSA | ?wxLANGUAGE_YIDDISH | ?wxLANGUAGE_YORUBA | ?wxLANGUAGE_ZHUANG | ?wxLANGUAGE_ZULU | ?wxLANGUAGE_USER_DEFINED | ?wxLANGUAGE_VALENCIAN | ?wxLANGUAGE_SAMI
-spec addCatalog(This, SzDomain, MsgIdLanguage, MsgIdCharset) -> boolean() when
	This::wxLocale(), SzDomain::unicode:chardata(), MsgIdLanguage::wx:wx_enum(), MsgIdCharset::unicode:chardata().
addCatalog(#wx_ref{type=ThisT,ref=ThisRef},SzDomain,MsgIdLanguage,MsgIdCharset)
 when is_list(SzDomain),is_integer(MsgIdLanguage),is_list(MsgIdCharset) ->
  ?CLASS(ThisT,wxLocale),
  SzDomain_UC = unicode:characters_to_binary([SzDomain,0]),
  MsgIdCharset_UC = unicode:characters_to_binary([MsgIdCharset,0]),
  wxe_util:call(?wxLocale_AddCatalog_3,
  <<ThisRef:32/?UI,(byte_size(SzDomain_UC)):32/?UI,(SzDomain_UC)/binary, 0:(((8- ((0+byte_size(SzDomain_UC)) band 16#7)) band 16#7))/unit:8,MsgIdLanguage:32/?UI,(byte_size(MsgIdCharset_UC)):32/?UI,(MsgIdCharset_UC)/binary, 0:(((8- ((0+byte_size(MsgIdCharset_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocaleaddcataloglookuppathprefix">external documentation</a>.
-spec addCatalogLookupPathPrefix(Prefix) -> 'ok' when
	Prefix::unicode:chardata().
addCatalogLookupPathPrefix(Prefix)
 when is_list(Prefix) ->
  Prefix_UC = unicode:characters_to_binary([Prefix,0]),
  wxe_util:cast(?wxLocale_AddCatalogLookupPathPrefix,
  <<(byte_size(Prefix_UC)):32/?UI,(Prefix_UC)/binary, 0:(((8- ((4+byte_size(Prefix_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalegetcanonicalname">external documentation</a>.
-spec getCanonicalName(This) -> unicode:charlist() when
	This::wxLocale().
getCanonicalName(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxLocale),
  wxe_util:call(?wxLocale_GetCanonicalName,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalegetlanguage">external documentation</a>.
-spec getLanguage(This) -> integer() when
	This::wxLocale().
getLanguage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxLocale),
  wxe_util:call(?wxLocale_GetLanguage,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalegetlanguagename">external documentation</a>.
-spec getLanguageName(Lang) -> unicode:charlist() when
	Lang::integer().
getLanguageName(Lang)
 when is_integer(Lang) ->
  wxe_util:call(?wxLocale_GetLanguageName,
  <<Lang:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalegetlocale">external documentation</a>.
-spec getLocale(This) -> unicode:charlist() when
	This::wxLocale().
getLocale(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxLocale),
  wxe_util:call(?wxLocale_GetLocale,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalegetname">external documentation</a>.
-spec getName(This) -> unicode:charlist() when
	This::wxLocale().
getName(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxLocale),
  wxe_util:call(?wxLocale_GetName,
  <<ThisRef:32/?UI>>).

%% @equiv getString(This,SzOrigString, [])
-spec getString(This, SzOrigString) -> unicode:charlist() when
	This::wxLocale(), SzOrigString::unicode:chardata().

getString(This,SzOrigString)
 when is_record(This, wx_ref),is_list(SzOrigString) ->
  getString(This,SzOrigString, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalegetstring">external documentation</a>.
-spec getString(This, SzOrigString, [Option]) -> unicode:charlist() when
	This::wxLocale(), SzOrigString::unicode:chardata(),
	Option :: {'szDomain', unicode:chardata()}.
getString(#wx_ref{type=ThisT,ref=ThisRef},SzOrigString, Options)
 when is_list(SzOrigString),is_list(Options) ->
  ?CLASS(ThisT,wxLocale),
  SzOrigString_UC = unicode:characters_to_binary([SzOrigString,0]),
  MOpts = fun({szDomain, SzDomain}, Acc) ->   SzDomain_UC = unicode:characters_to_binary([SzDomain,0]),[<<1:32/?UI,(byte_size(SzDomain_UC)):32/?UI,(SzDomain_UC)/binary, 0:(((8- ((0+byte_size(SzDomain_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxLocale_GetString_2,
  <<ThisRef:32/?UI,(byte_size(SzOrigString_UC)):32/?UI,(SzOrigString_UC)/binary, 0:(((8- ((0+byte_size(SzOrigString_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @equiv getString(This,SzOrigString,SzOrigString2,N, [])
-spec getString(This, SzOrigString, SzOrigString2, N) -> unicode:charlist() when
	This::wxLocale(), SzOrigString::unicode:chardata(), SzOrigString2::unicode:chardata(), N::integer().

getString(This,SzOrigString,SzOrigString2,N)
 when is_record(This, wx_ref),is_list(SzOrigString),is_list(SzOrigString2),is_integer(N) ->
  getString(This,SzOrigString,SzOrigString2,N, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalegetstring">external documentation</a>.
-spec getString(This, SzOrigString, SzOrigString2, N, [Option]) -> unicode:charlist() when
	This::wxLocale(), SzOrigString::unicode:chardata(), SzOrigString2::unicode:chardata(), N::integer(),
	Option :: {'szDomain', unicode:chardata()}.
getString(#wx_ref{type=ThisT,ref=ThisRef},SzOrigString,SzOrigString2,N, Options)
 when is_list(SzOrigString),is_list(SzOrigString2),is_integer(N),is_list(Options) ->
  ?CLASS(ThisT,wxLocale),
  SzOrigString_UC = unicode:characters_to_binary([SzOrigString,0]),
  SzOrigString2_UC = unicode:characters_to_binary([SzOrigString2,0]),
  MOpts = fun({szDomain, SzDomain}, Acc) ->   SzDomain_UC = unicode:characters_to_binary([SzDomain,0]),[<<1:32/?UI,(byte_size(SzDomain_UC)):32/?UI,(SzDomain_UC)/binary, 0:(((8- ((0+byte_size(SzDomain_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxLocale_GetString_4,
  <<ThisRef:32/?UI,(byte_size(SzOrigString_UC)):32/?UI,(SzOrigString_UC)/binary, 0:(((8- ((0+byte_size(SzOrigString_UC)) band 16#7)) band 16#7))/unit:8,(byte_size(SzOrigString2_UC)):32/?UI,(SzOrigString2_UC)/binary, 0:(((8- ((4+byte_size(SzOrigString2_UC)) band 16#7)) band 16#7))/unit:8,N:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv getHeaderValue(This,SzHeader, [])
-spec getHeaderValue(This, SzHeader) -> unicode:charlist() when
	This::wxLocale(), SzHeader::unicode:chardata().

getHeaderValue(This,SzHeader)
 when is_record(This, wx_ref),is_list(SzHeader) ->
  getHeaderValue(This,SzHeader, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalegetheadervalue">external documentation</a>.
-spec getHeaderValue(This, SzHeader, [Option]) -> unicode:charlist() when
	This::wxLocale(), SzHeader::unicode:chardata(),
	Option :: {'szDomain', unicode:chardata()}.
getHeaderValue(#wx_ref{type=ThisT,ref=ThisRef},SzHeader, Options)
 when is_list(SzHeader),is_list(Options) ->
  ?CLASS(ThisT,wxLocale),
  SzHeader_UC = unicode:characters_to_binary([SzHeader,0]),
  MOpts = fun({szDomain, SzDomain}, Acc) ->   SzDomain_UC = unicode:characters_to_binary([SzDomain,0]),[<<1:32/?UI,(byte_size(SzDomain_UC)):32/?UI,(SzDomain_UC)/binary, 0:(((8- ((0+byte_size(SzDomain_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxLocale_GetHeaderValue,
  <<ThisRef:32/?UI,(byte_size(SzHeader_UC)):32/?UI,(SzHeader_UC)/binary, 0:(((8- ((0+byte_size(SzHeader_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalegetsysname">external documentation</a>.
-spec getSysName(This) -> unicode:charlist() when
	This::wxLocale().
getSysName(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxLocale),
  wxe_util:call(?wxLocale_GetSysName,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalegetsystemencoding">external documentation</a>.
%%<br /> Res = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS
-spec getSystemEncoding() -> wx:wx_enum().
getSystemEncoding() ->
  wxe_util:call(?wxLocale_GetSystemEncoding,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalegetsystemencodingname">external documentation</a>.
-spec getSystemEncodingName() -> unicode:charlist().
getSystemEncodingName() ->
  wxe_util:call(?wxLocale_GetSystemEncodingName,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocalegetsystemlanguage">external documentation</a>.
-spec getSystemLanguage() -> integer().
getSystemLanguage() ->
  wxe_util:call(?wxLocale_GetSystemLanguage,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocaleisloaded">external documentation</a>.
-spec isLoaded(This, SzDomain) -> boolean() when
	This::wxLocale(), SzDomain::unicode:chardata().
isLoaded(#wx_ref{type=ThisT,ref=ThisRef},SzDomain)
 when is_list(SzDomain) ->
  ?CLASS(ThisT,wxLocale),
  SzDomain_UC = unicode:characters_to_binary([SzDomain,0]),
  wxe_util:call(?wxLocale_IsLoaded,
  <<ThisRef:32/?UI,(byte_size(SzDomain_UC)):32/?UI,(SzDomain_UC)/binary, 0:(((8- ((0+byte_size(SzDomain_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlocale.html#wxlocaleisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxLocale().
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxLocale),
  wxe_util:call(?wxLocale_IsOk,
  <<ThisRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxLocale()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxLocale),
  wxe_util:destroy(?wxLocale_destruct,Obj),
  ok.
