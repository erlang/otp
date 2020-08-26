/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2020. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
*/
 /* This file is also generated */
#include <wx/wx.h>
#include "../wxe_impl.h"
#include "wxe_macros.h"
#include "../wxe_return.h"
void WxeApp::init_nonconsts(wxeMemEnv *memenv, ErlNifPid caller) {
  WxeApp * app = this;
  wxeReturn rt = wxeReturn(memenv, caller, false);
  ERL_NIF_TERM consts[] = {
    enif_make_tuple2(rt.env, rt.make_atom("wxMAJOR_VERSION"), rt.make_int(wxMAJOR_VERSION)),
    enif_make_tuple2(rt.env, rt.make_atom("wxMINOR_VERSION"), rt.make_int(wxMINOR_VERSION)),
    enif_make_tuple2(rt.env, rt.make_atom("wxRELEASE_NUMBER"), rt.make_int(wxRELEASE_NUMBER)),
    enif_make_tuple2(rt.env, rt.make_atom("wxRETAINED"), rt.make_int(wxRETAINED)),
    enif_make_tuple2(rt.env, rt.make_atom("wxSUBRELEASE_NUMBER"), rt.make_int(wxSUBRELEASE_NUMBER)),
    enif_make_tuple2(rt.env, rt.make_atom("wxSL_LABELS"), rt.make_int(wxSL_LABELS)),
    enif_make_tuple2(rt.env, rt.make_atom("wxTR_DEFAULT_STYLE"), rt.make_int(wxTR_DEFAULT_STYLE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxFD_MULTIPLE"), rt.make_int(wxFD_MULTIPLE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxBK_HITTEST_ONITEM"), rt.make_int(wxBK_HITTEST_ONITEM)),
    enif_make_tuple2(rt.env, rt.make_atom("wxCAL_SUNDAY_FIRST"), rt.make_int(wxCAL_SUNDAY_FIRST)),
    enif_make_tuple2(rt.env, rt.make_atom("wxBG_STYLE_COLOUR"), rt.make_int(wxBG_STYLE_COLOUR)),
    enif_make_tuple2(rt.env, rt.make_atom("wxBG_STYLE_ERASE"), rt.make_int(wxBG_STYLE_ERASE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxBG_STYLE_PAINT"), rt.make_int(wxBG_STYLE_PAINT)),
    enif_make_tuple2(rt.env, rt.make_atom("wxBG_STYLE_SYSTEM"), rt.make_int(wxBG_STYLE_SYSTEM)),
    enif_make_tuple2(rt.env, rt.make_atom("wxBG_STYLE_TRANSPARENT"), rt.make_int(wxBG_STYLE_TRANSPARENT)),
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTENCODING_UNICODE"), rt.make_int(wxFONTENCODING_UNICODE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTENCODING_UTF16"), rt.make_int(wxFONTENCODING_UTF16)),
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTENCODING_UTF32"), rt.make_int(wxFONTENCODING_UTF32)),
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_BOLD"), rt.make_int(wxFONTWEIGHT_BOLD)),
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_LIGHT"), rt.make_int(wxFONTWEIGHT_LIGHT)),
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_MAX"), rt.make_int(wxFONTWEIGHT_MAX)),
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_NORMAL"), rt.make_int(wxFONTWEIGHT_NORMAL)),
    enif_make_tuple2(rt.env, rt.make_atom("wxIMAGE_QUALITY_BICUBIC"), rt.make_int(wxIMAGE_QUALITY_BICUBIC)),
    enif_make_tuple2(rt.env, rt.make_atom("wxIMAGE_QUALITY_BILINEAR"), rt.make_int(wxIMAGE_QUALITY_BILINEAR)),
    enif_make_tuple2(rt.env, rt.make_atom("wxIMAGE_QUALITY_BOX_AVERAGE"), rt.make_int(wxIMAGE_QUALITY_BOX_AVERAGE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxIMAGE_QUALITY_HIGH"), rt.make_int(wxIMAGE_QUALITY_HIGH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxIMAGE_QUALITY_NEAREST"), rt.make_int(wxIMAGE_QUALITY_NEAREST)),
    enif_make_tuple2(rt.env, rt.make_atom("wxIMAGE_QUALITY_NORMAL"), rt.make_int(wxIMAGE_QUALITY_NORMAL)),
    enif_make_tuple2(rt.env, rt.make_atom("wxMOD_CMD"), rt.make_int(wxMOD_CMD)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ABKHAZIAN"), rt.make_int(wxLANGUAGE_ABKHAZIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_AFAR"), rt.make_int(wxLANGUAGE_AFAR)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_AFRIKAANS"), rt.make_int(wxLANGUAGE_AFRIKAANS)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ALBANIAN"), rt.make_int(wxLANGUAGE_ALBANIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_AMHARIC"), rt.make_int(wxLANGUAGE_AMHARIC)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC"), rt.make_int(wxLANGUAGE_ARABIC)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_ALGERIA"), rt.make_int(wxLANGUAGE_ARABIC_ALGERIA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_BAHRAIN"), rt.make_int(wxLANGUAGE_ARABIC_BAHRAIN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_EGYPT"), rt.make_int(wxLANGUAGE_ARABIC_EGYPT)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_IRAQ"), rt.make_int(wxLANGUAGE_ARABIC_IRAQ)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_JORDAN"), rt.make_int(wxLANGUAGE_ARABIC_JORDAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_KUWAIT"), rt.make_int(wxLANGUAGE_ARABIC_KUWAIT)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_LEBANON"), rt.make_int(wxLANGUAGE_ARABIC_LEBANON)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_LIBYA"), rt.make_int(wxLANGUAGE_ARABIC_LIBYA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_MOROCCO"), rt.make_int(wxLANGUAGE_ARABIC_MOROCCO)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_OMAN"), rt.make_int(wxLANGUAGE_ARABIC_OMAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_QATAR"), rt.make_int(wxLANGUAGE_ARABIC_QATAR)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_SAUDI_ARABIA"), rt.make_int(wxLANGUAGE_ARABIC_SAUDI_ARABIA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_SUDAN"), rt.make_int(wxLANGUAGE_ARABIC_SUDAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_SYRIA"), rt.make_int(wxLANGUAGE_ARABIC_SYRIA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_TUNISIA"), rt.make_int(wxLANGUAGE_ARABIC_TUNISIA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_UAE"), rt.make_int(wxLANGUAGE_ARABIC_UAE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARABIC_YEMEN"), rt.make_int(wxLANGUAGE_ARABIC_YEMEN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ARMENIAN"), rt.make_int(wxLANGUAGE_ARMENIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ASSAMESE"), rt.make_int(wxLANGUAGE_ASSAMESE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ASTURIAN"), rt.make_int(wxLANGUAGE_ASTURIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_AYMARA"), rt.make_int(wxLANGUAGE_AYMARA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_AZERI"), rt.make_int(wxLANGUAGE_AZERI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_AZERI_CYRILLIC"), rt.make_int(wxLANGUAGE_AZERI_CYRILLIC)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_AZERI_LATIN"), rt.make_int(wxLANGUAGE_AZERI_LATIN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_BASHKIR"), rt.make_int(wxLANGUAGE_BASHKIR)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_BASQUE"), rt.make_int(wxLANGUAGE_BASQUE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_BELARUSIAN"), rt.make_int(wxLANGUAGE_BELARUSIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_BENGALI"), rt.make_int(wxLANGUAGE_BENGALI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_BHUTANI"), rt.make_int(wxLANGUAGE_BHUTANI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_BIHARI"), rt.make_int(wxLANGUAGE_BIHARI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_BISLAMA"), rt.make_int(wxLANGUAGE_BISLAMA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_BOSNIAN"), rt.make_int(wxLANGUAGE_BOSNIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_BRETON"), rt.make_int(wxLANGUAGE_BRETON)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_BULGARIAN"), rt.make_int(wxLANGUAGE_BULGARIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_BURMESE"), rt.make_int(wxLANGUAGE_BURMESE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_CAMBODIAN"), rt.make_int(wxLANGUAGE_CAMBODIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_CATALAN"), rt.make_int(wxLANGUAGE_CATALAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_CHINESE"), rt.make_int(wxLANGUAGE_CHINESE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_CHINESE_HONGKONG"), rt.make_int(wxLANGUAGE_CHINESE_HONGKONG)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_CHINESE_MACAU"), rt.make_int(wxLANGUAGE_CHINESE_MACAU)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_CHINESE_SIMPLIFIED"), rt.make_int(wxLANGUAGE_CHINESE_SIMPLIFIED)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_CHINESE_SINGAPORE"), rt.make_int(wxLANGUAGE_CHINESE_SINGAPORE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_CHINESE_TAIWAN"), rt.make_int(wxLANGUAGE_CHINESE_TAIWAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_CHINESE_TRADITIONAL"), rt.make_int(wxLANGUAGE_CHINESE_TRADITIONAL)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_CORSICAN"), rt.make_int(wxLANGUAGE_CORSICAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_CROATIAN"), rt.make_int(wxLANGUAGE_CROATIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_CZECH"), rt.make_int(wxLANGUAGE_CZECH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_DANISH"), rt.make_int(wxLANGUAGE_DANISH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_DEFAULT"), rt.make_int(wxLANGUAGE_DEFAULT)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_DUTCH"), rt.make_int(wxLANGUAGE_DUTCH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_DUTCH_BELGIAN"), rt.make_int(wxLANGUAGE_DUTCH_BELGIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH"), rt.make_int(wxLANGUAGE_ENGLISH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_AUSTRALIA"), rt.make_int(wxLANGUAGE_ENGLISH_AUSTRALIA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_BELIZE"), rt.make_int(wxLANGUAGE_ENGLISH_BELIZE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_BOTSWANA"), rt.make_int(wxLANGUAGE_ENGLISH_BOTSWANA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_CANADA"), rt.make_int(wxLANGUAGE_ENGLISH_CANADA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_CARIBBEAN"), rt.make_int(wxLANGUAGE_ENGLISH_CARIBBEAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_DENMARK"), rt.make_int(wxLANGUAGE_ENGLISH_DENMARK)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_EIRE"), rt.make_int(wxLANGUAGE_ENGLISH_EIRE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_JAMAICA"), rt.make_int(wxLANGUAGE_ENGLISH_JAMAICA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_NEW_ZEALAND"), rt.make_int(wxLANGUAGE_ENGLISH_NEW_ZEALAND)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_PHILIPPINES"), rt.make_int(wxLANGUAGE_ENGLISH_PHILIPPINES)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_SOUTH_AFRICA"), rt.make_int(wxLANGUAGE_ENGLISH_SOUTH_AFRICA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_TRINIDAD"), rt.make_int(wxLANGUAGE_ENGLISH_TRINIDAD)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_UK"), rt.make_int(wxLANGUAGE_ENGLISH_UK)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_US"), rt.make_int(wxLANGUAGE_ENGLISH_US)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ENGLISH_ZIMBABWE"), rt.make_int(wxLANGUAGE_ENGLISH_ZIMBABWE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ESPERANTO"), rt.make_int(wxLANGUAGE_ESPERANTO)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ESTONIAN"), rt.make_int(wxLANGUAGE_ESTONIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_FAEROESE"), rt.make_int(wxLANGUAGE_FAEROESE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_FARSI"), rt.make_int(wxLANGUAGE_FARSI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_FIJI"), rt.make_int(wxLANGUAGE_FIJI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_FINNISH"), rt.make_int(wxLANGUAGE_FINNISH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_FRENCH"), rt.make_int(wxLANGUAGE_FRENCH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_FRENCH_BELGIAN"), rt.make_int(wxLANGUAGE_FRENCH_BELGIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_FRENCH_CANADIAN"), rt.make_int(wxLANGUAGE_FRENCH_CANADIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_FRENCH_LUXEMBOURG"), rt.make_int(wxLANGUAGE_FRENCH_LUXEMBOURG)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_FRENCH_MONACO"), rt.make_int(wxLANGUAGE_FRENCH_MONACO)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_FRENCH_SWISS"), rt.make_int(wxLANGUAGE_FRENCH_SWISS)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_FRISIAN"), rt.make_int(wxLANGUAGE_FRISIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_GALICIAN"), rt.make_int(wxLANGUAGE_GALICIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_GEORGIAN"), rt.make_int(wxLANGUAGE_GEORGIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_GERMAN"), rt.make_int(wxLANGUAGE_GERMAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_GERMAN_AUSTRIAN"), rt.make_int(wxLANGUAGE_GERMAN_AUSTRIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_GERMAN_BELGIUM"), rt.make_int(wxLANGUAGE_GERMAN_BELGIUM)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_GERMAN_LIECHTENSTEIN"), rt.make_int(wxLANGUAGE_GERMAN_LIECHTENSTEIN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_GERMAN_LUXEMBOURG"), rt.make_int(wxLANGUAGE_GERMAN_LUXEMBOURG)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_GERMAN_SWISS"), rt.make_int(wxLANGUAGE_GERMAN_SWISS)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_GREEK"), rt.make_int(wxLANGUAGE_GREEK)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_GREENLANDIC"), rt.make_int(wxLANGUAGE_GREENLANDIC)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_GUARANI"), rt.make_int(wxLANGUAGE_GUARANI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_GUJARATI"), rt.make_int(wxLANGUAGE_GUJARATI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_HAUSA"), rt.make_int(wxLANGUAGE_HAUSA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_HEBREW"), rt.make_int(wxLANGUAGE_HEBREW)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_HINDI"), rt.make_int(wxLANGUAGE_HINDI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_HUNGARIAN"), rt.make_int(wxLANGUAGE_HUNGARIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ICELANDIC"), rt.make_int(wxLANGUAGE_ICELANDIC)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_INDONESIAN"), rt.make_int(wxLANGUAGE_INDONESIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_INTERLINGUA"), rt.make_int(wxLANGUAGE_INTERLINGUA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_INTERLINGUE"), rt.make_int(wxLANGUAGE_INTERLINGUE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_INUKTITUT"), rt.make_int(wxLANGUAGE_INUKTITUT)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_INUPIAK"), rt.make_int(wxLANGUAGE_INUPIAK)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_IRISH"), rt.make_int(wxLANGUAGE_IRISH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ITALIAN"), rt.make_int(wxLANGUAGE_ITALIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ITALIAN_SWISS"), rt.make_int(wxLANGUAGE_ITALIAN_SWISS)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_JAPANESE"), rt.make_int(wxLANGUAGE_JAPANESE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_JAVANESE"), rt.make_int(wxLANGUAGE_JAVANESE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_KABYLE"), rt.make_int(wxLANGUAGE_KABYLE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_KANNADA"), rt.make_int(wxLANGUAGE_KANNADA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_KASHMIRI"), rt.make_int(wxLANGUAGE_KASHMIRI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_KASHMIRI_INDIA"), rt.make_int(wxLANGUAGE_KASHMIRI_INDIA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_KAZAKH"), rt.make_int(wxLANGUAGE_KAZAKH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_KERNEWEK"), rt.make_int(wxLANGUAGE_KERNEWEK)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_KINYARWANDA"), rt.make_int(wxLANGUAGE_KINYARWANDA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_KIRGHIZ"), rt.make_int(wxLANGUAGE_KIRGHIZ)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_KIRUNDI"), rt.make_int(wxLANGUAGE_KIRUNDI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_KONKANI"), rt.make_int(wxLANGUAGE_KONKANI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_KOREAN"), rt.make_int(wxLANGUAGE_KOREAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_KURDISH"), rt.make_int(wxLANGUAGE_KURDISH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_LAOTHIAN"), rt.make_int(wxLANGUAGE_LAOTHIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_LATIN"), rt.make_int(wxLANGUAGE_LATIN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_LATVIAN"), rt.make_int(wxLANGUAGE_LATVIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_LINGALA"), rt.make_int(wxLANGUAGE_LINGALA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_LITHUANIAN"), rt.make_int(wxLANGUAGE_LITHUANIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_MACEDONIAN"), rt.make_int(wxLANGUAGE_MACEDONIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_MALAGASY"), rt.make_int(wxLANGUAGE_MALAGASY)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_MALAY"), rt.make_int(wxLANGUAGE_MALAY)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_MALAYALAM"), rt.make_int(wxLANGUAGE_MALAYALAM)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_MALAY_BRUNEI_DARUSSALAM"), rt.make_int(wxLANGUAGE_MALAY_BRUNEI_DARUSSALAM)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_MALAY_MALAYSIA"), rt.make_int(wxLANGUAGE_MALAY_MALAYSIA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_MALTESE"), rt.make_int(wxLANGUAGE_MALTESE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_MANIPURI"), rt.make_int(wxLANGUAGE_MANIPURI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_MAORI"), rt.make_int(wxLANGUAGE_MAORI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_MARATHI"), rt.make_int(wxLANGUAGE_MARATHI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_MOLDAVIAN"), rt.make_int(wxLANGUAGE_MOLDAVIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_MONGOLIAN"), rt.make_int(wxLANGUAGE_MONGOLIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_NAURU"), rt.make_int(wxLANGUAGE_NAURU)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_NEPALI"), rt.make_int(wxLANGUAGE_NEPALI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_NEPALI_INDIA"), rt.make_int(wxLANGUAGE_NEPALI_INDIA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_NORWEGIAN_BOKMAL"), rt.make_int(wxLANGUAGE_NORWEGIAN_BOKMAL)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_NORWEGIAN_NYNORSK"), rt.make_int(wxLANGUAGE_NORWEGIAN_NYNORSK)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_OCCITAN"), rt.make_int(wxLANGUAGE_OCCITAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ORIYA"), rt.make_int(wxLANGUAGE_ORIYA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_OROMO"), rt.make_int(wxLANGUAGE_OROMO)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_PASHTO"), rt.make_int(wxLANGUAGE_PASHTO)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_POLISH"), rt.make_int(wxLANGUAGE_POLISH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_PORTUGUESE"), rt.make_int(wxLANGUAGE_PORTUGUESE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_PORTUGUESE_BRAZILIAN"), rt.make_int(wxLANGUAGE_PORTUGUESE_BRAZILIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_PUNJABI"), rt.make_int(wxLANGUAGE_PUNJABI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_QUECHUA"), rt.make_int(wxLANGUAGE_QUECHUA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_RHAETO_ROMANCE"), rt.make_int(wxLANGUAGE_RHAETO_ROMANCE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ROMANIAN"), rt.make_int(wxLANGUAGE_ROMANIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_RUSSIAN"), rt.make_int(wxLANGUAGE_RUSSIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_RUSSIAN_UKRAINE"), rt.make_int(wxLANGUAGE_RUSSIAN_UKRAINE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SAMI"), rt.make_int(wxLANGUAGE_SAMI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SAMOAN"), rt.make_int(wxLANGUAGE_SAMOAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SANGHO"), rt.make_int(wxLANGUAGE_SANGHO)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SANSKRIT"), rt.make_int(wxLANGUAGE_SANSKRIT)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SCOTS_GAELIC"), rt.make_int(wxLANGUAGE_SCOTS_GAELIC)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SERBIAN"), rt.make_int(wxLANGUAGE_SERBIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SERBIAN_CYRILLIC"), rt.make_int(wxLANGUAGE_SERBIAN_CYRILLIC)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SERBIAN_LATIN"), rt.make_int(wxLANGUAGE_SERBIAN_LATIN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SERBO_CROATIAN"), rt.make_int(wxLANGUAGE_SERBO_CROATIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SESOTHO"), rt.make_int(wxLANGUAGE_SESOTHO)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SETSWANA"), rt.make_int(wxLANGUAGE_SETSWANA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SHONA"), rt.make_int(wxLANGUAGE_SHONA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SINDHI"), rt.make_int(wxLANGUAGE_SINDHI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SINHALESE"), rt.make_int(wxLANGUAGE_SINHALESE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SISWATI"), rt.make_int(wxLANGUAGE_SISWATI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SLOVAK"), rt.make_int(wxLANGUAGE_SLOVAK)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SLOVENIAN"), rt.make_int(wxLANGUAGE_SLOVENIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SOMALI"), rt.make_int(wxLANGUAGE_SOMALI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH"), rt.make_int(wxLANGUAGE_SPANISH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_ARGENTINA"), rt.make_int(wxLANGUAGE_SPANISH_ARGENTINA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_BOLIVIA"), rt.make_int(wxLANGUAGE_SPANISH_BOLIVIA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_CHILE"), rt.make_int(wxLANGUAGE_SPANISH_CHILE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_COLOMBIA"), rt.make_int(wxLANGUAGE_SPANISH_COLOMBIA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_COSTA_RICA"), rt.make_int(wxLANGUAGE_SPANISH_COSTA_RICA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_DOMINICAN_REPUBLIC"), rt.make_int(wxLANGUAGE_SPANISH_DOMINICAN_REPUBLIC)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_ECUADOR"), rt.make_int(wxLANGUAGE_SPANISH_ECUADOR)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_EL_SALVADOR"), rt.make_int(wxLANGUAGE_SPANISH_EL_SALVADOR)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_GUATEMALA"), rt.make_int(wxLANGUAGE_SPANISH_GUATEMALA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_HONDURAS"), rt.make_int(wxLANGUAGE_SPANISH_HONDURAS)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_MEXICAN"), rt.make_int(wxLANGUAGE_SPANISH_MEXICAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_MODERN"), rt.make_int(wxLANGUAGE_SPANISH_MODERN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_NICARAGUA"), rt.make_int(wxLANGUAGE_SPANISH_NICARAGUA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_PANAMA"), rt.make_int(wxLANGUAGE_SPANISH_PANAMA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_PARAGUAY"), rt.make_int(wxLANGUAGE_SPANISH_PARAGUAY)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_PERU"), rt.make_int(wxLANGUAGE_SPANISH_PERU)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_PUERTO_RICO"), rt.make_int(wxLANGUAGE_SPANISH_PUERTO_RICO)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_URUGUAY"), rt.make_int(wxLANGUAGE_SPANISH_URUGUAY)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_US"), rt.make_int(wxLANGUAGE_SPANISH_US)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SPANISH_VENEZUELA"), rt.make_int(wxLANGUAGE_SPANISH_VENEZUELA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SUNDANESE"), rt.make_int(wxLANGUAGE_SUNDANESE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SWAHILI"), rt.make_int(wxLANGUAGE_SWAHILI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SWEDISH"), rt.make_int(wxLANGUAGE_SWEDISH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_SWEDISH_FINLAND"), rt.make_int(wxLANGUAGE_SWEDISH_FINLAND)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_TAGALOG"), rt.make_int(wxLANGUAGE_TAGALOG)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_TAJIK"), rt.make_int(wxLANGUAGE_TAJIK)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_TAMIL"), rt.make_int(wxLANGUAGE_TAMIL)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_TATAR"), rt.make_int(wxLANGUAGE_TATAR)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_TELUGU"), rt.make_int(wxLANGUAGE_TELUGU)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_THAI"), rt.make_int(wxLANGUAGE_THAI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_TIBETAN"), rt.make_int(wxLANGUAGE_TIBETAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_TIGRINYA"), rt.make_int(wxLANGUAGE_TIGRINYA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_TONGA"), rt.make_int(wxLANGUAGE_TONGA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_TSONGA"), rt.make_int(wxLANGUAGE_TSONGA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_TURKISH"), rt.make_int(wxLANGUAGE_TURKISH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_TURKMEN"), rt.make_int(wxLANGUAGE_TURKMEN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_TWI"), rt.make_int(wxLANGUAGE_TWI)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_UIGHUR"), rt.make_int(wxLANGUAGE_UIGHUR)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_UKRAINIAN"), rt.make_int(wxLANGUAGE_UKRAINIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_UNKNOWN"), rt.make_int(wxLANGUAGE_UNKNOWN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_URDU"), rt.make_int(wxLANGUAGE_URDU)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_URDU_INDIA"), rt.make_int(wxLANGUAGE_URDU_INDIA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_URDU_PAKISTAN"), rt.make_int(wxLANGUAGE_URDU_PAKISTAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_USER_DEFINED"), rt.make_int(wxLANGUAGE_USER_DEFINED)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_UZBEK"), rt.make_int(wxLANGUAGE_UZBEK)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_UZBEK_CYRILLIC"), rt.make_int(wxLANGUAGE_UZBEK_CYRILLIC)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_UZBEK_LATIN"), rt.make_int(wxLANGUAGE_UZBEK_LATIN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_VALENCIAN"), rt.make_int(wxLANGUAGE_VALENCIAN)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_VIETNAMESE"), rt.make_int(wxLANGUAGE_VIETNAMESE)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_VOLAPUK"), rt.make_int(wxLANGUAGE_VOLAPUK)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_WELSH"), rt.make_int(wxLANGUAGE_WELSH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_WOLOF"), rt.make_int(wxLANGUAGE_WOLOF)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_XHOSA"), rt.make_int(wxLANGUAGE_XHOSA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_YIDDISH"), rt.make_int(wxLANGUAGE_YIDDISH)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_YORUBA"), rt.make_int(wxLANGUAGE_YORUBA)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ZHUANG"), rt.make_int(wxLANGUAGE_ZHUANG)),
    enif_make_tuple2(rt.env, rt.make_atom("wxLANGUAGE_ZULU"), rt.make_int(wxLANGUAGE_ZULU)),
    enif_make_tuple2(rt.env, rt.make_atom("wxCURSOR_ARROWWAIT"), rt.make_int(wxCURSOR_ARROWWAIT)),
    enif_make_tuple2(rt.env, rt.make_atom("wxCURSOR_DEFAULT"), rt.make_int(wxCURSOR_DEFAULT)),
    enif_make_tuple2(rt.env, rt.make_atom("wxCURSOR_MAX"), rt.make_int(wxCURSOR_MAX)),
#if wxCHECK_VERSION(3,0,3)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_CORE_PROFILE"), rt.make_int(WX_GL_CORE_PROFILE)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_CORE_PROFILE"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,0)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_DEBUG"), rt.make_int(WX_GL_DEBUG)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_DEBUG"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,0)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_ES2"), rt.make_int(WX_GL_ES2)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_ES2"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,0)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_FORWARD_COMPAT"), rt.make_int(WX_GL_FORWARD_COMPAT)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_FORWARD_COMPAT"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,0)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_FRAMEBUFFER_SRGB"), rt.make_int(WX_GL_FRAMEBUFFER_SRGB)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_FRAMEBUFFER_SRGB"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,0)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_LOSE_ON_RESET"), rt.make_int(WX_GL_LOSE_ON_RESET)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_LOSE_ON_RESET"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,0,3)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_MAJOR_VERSION"), rt.make_int(WX_GL_MAJOR_VERSION)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_MAJOR_VERSION"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,0,3)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_MINOR_VERSION"), rt.make_int(WX_GL_MINOR_VERSION)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_MINOR_VERSION"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,0)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_NO_RESET_NOTIFY"), rt.make_int(WX_GL_NO_RESET_NOTIFY)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_NO_RESET_NOTIFY"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,0)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_RELEASE_FLUSH"), rt.make_int(WX_GL_RELEASE_FLUSH)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_RELEASE_FLUSH"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,0)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_RELEASE_NONE"), rt.make_int(WX_GL_RELEASE_NONE)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_RELEASE_NONE"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,0)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_RESET_ISOLATION"), rt.make_int(WX_GL_RESET_ISOLATION)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_RESET_ISOLATION"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,0)
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_ROBUST_ACCESS"), rt.make_int(WX_GL_ROBUST_ACCESS)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("WX_GL_ROBUST_ACCESS"), WXE_ATOM_undefined),
#endif
    enif_make_tuple2(rt.env, rt.make_atom("wxBLACK"), rt.make(*(wxBLACK))),
    enif_make_tuple2(rt.env, rt.make_atom("wxBLACK_BRUSH"),rt.make_ref(app->getRef((void *)wxBLACK_BRUSH,memenv), "wxBrush")),
    enif_make_tuple2(rt.env, rt.make_atom("wxBLACK_DASHED_PEN"),rt.make_ref(app->getRef((void *)wxBLACK_DASHED_PEN,memenv), "wxPen")),
    enif_make_tuple2(rt.env, rt.make_atom("wxBLACK_PEN"),rt.make_ref(app->getRef((void *)wxBLACK_PEN,memenv), "wxPen")),
    enif_make_tuple2(rt.env, rt.make_atom("wxBLUE"), rt.make(*(wxBLUE))),
    enif_make_tuple2(rt.env, rt.make_atom("wxBLUE_BRUSH"),rt.make_ref(app->getRef((void *)wxBLUE_BRUSH,memenv), "wxBrush")),
    enif_make_tuple2(rt.env, rt.make_atom("wxCROSS_CURSOR"),rt.make_ref(app->getRef((void *)wxCROSS_CURSOR,memenv), "wxCursor")),
    enif_make_tuple2(rt.env, rt.make_atom("wxCYAN"), rt.make(*(wxCYAN))),
    enif_make_tuple2(rt.env, rt.make_atom("wxCYAN_BRUSH"),rt.make_ref(app->getRef((void *)wxCYAN_BRUSH,memenv), "wxBrush")),
    enif_make_tuple2(rt.env, rt.make_atom("wxCYAN_PEN"),rt.make_ref(app->getRef((void *)wxCYAN_PEN,memenv), "wxPen")),
#if wxCHECK_VERSION(3,1,2)
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_EXTRABOLD"), rt.make_int(wxFONTWEIGHT_EXTRABOLD)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_EXTRABOLD"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,2)
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_EXTRAHEAVY"), rt.make_int(wxFONTWEIGHT_EXTRAHEAVY)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_EXTRAHEAVY"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,2)
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_EXTRALIGHT"), rt.make_int(wxFONTWEIGHT_EXTRALIGHT)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_EXTRALIGHT"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,2)
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_HEAVY"), rt.make_int(wxFONTWEIGHT_HEAVY)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_HEAVY"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,2)
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_INVALID"), rt.make_int(wxFONTWEIGHT_INVALID)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_INVALID"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,2)
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_MEDIUM"), rt.make_int(wxFONTWEIGHT_MEDIUM)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_MEDIUM"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,2)
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_SEMIBOLD"), rt.make_int(wxFONTWEIGHT_SEMIBOLD)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_SEMIBOLD"), WXE_ATOM_undefined),
#endif
#if wxCHECK_VERSION(3,1,2)
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_THIN"), rt.make_int(wxFONTWEIGHT_THIN)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("wxFONTWEIGHT_THIN"), WXE_ATOM_undefined),
#endif
    enif_make_tuple2(rt.env, rt.make_atom("wxGREEN"), rt.make(*(wxGREEN))),
    enif_make_tuple2(rt.env, rt.make_atom("wxGREEN_BRUSH"),rt.make_ref(app->getRef((void *)wxGREEN_BRUSH,memenv), "wxBrush")),
    enif_make_tuple2(rt.env, rt.make_atom("wxGREEN_PEN"),rt.make_ref(app->getRef((void *)wxGREEN_PEN,memenv), "wxPen")),
    enif_make_tuple2(rt.env, rt.make_atom("wxGREY_BRUSH"),rt.make_ref(app->getRef((void *)wxGREY_BRUSH,memenv), "wxBrush")),
    enif_make_tuple2(rt.env, rt.make_atom("wxGREY_PEN"),rt.make_ref(app->getRef((void *)wxGREY_PEN,memenv), "wxPen")),
    enif_make_tuple2(rt.env, rt.make_atom("wxHOURGLASS_CURSOR"),rt.make_ref(app->getRef((void *)wxHOURGLASS_CURSOR,memenv), "wxCursor")),
    enif_make_tuple2(rt.env, rt.make_atom("wxITALIC_FONT"),rt.make_ref(app->getRef((void *)wxITALIC_FONT,memenv), "wxFont")),
    enif_make_tuple2(rt.env, rt.make_atom("wxLIGHT_GREY"), rt.make(*(wxLIGHT_GREY))),
    enif_make_tuple2(rt.env, rt.make_atom("wxLIGHT_GREY_BRUSH"),rt.make_ref(app->getRef((void *)wxLIGHT_GREY_BRUSH,memenv), "wxBrush")),
    enif_make_tuple2(rt.env, rt.make_atom("wxLIGHT_GREY_PEN"),rt.make_ref(app->getRef((void *)wxLIGHT_GREY_PEN,memenv), "wxPen")),
    enif_make_tuple2(rt.env, rt.make_atom("wxMEDIUM_GREY_BRUSH"),rt.make_ref(app->getRef((void *)wxMEDIUM_GREY_BRUSH,memenv), "wxBrush")),
    enif_make_tuple2(rt.env, rt.make_atom("wxMEDIUM_GREY_PEN"),rt.make_ref(app->getRef((void *)wxMEDIUM_GREY_PEN,memenv), "wxPen")),
    enif_make_tuple2(rt.env, rt.make_atom("wxNORMAL_FONT"),rt.make_ref(app->getRef((void *)wxNORMAL_FONT,memenv), "wxFont")),
    enif_make_tuple2(rt.env, rt.make_atom("wxNullBitmap"),rt.make_ref(app->getRef((void *)&wxNullBitmap,memenv), "wxBitmap")),
    enif_make_tuple2(rt.env, rt.make_atom("wxNullBrush"),rt.make_ref(app->getRef((void *)&wxNullBrush,memenv), "wxBrush")),
    enif_make_tuple2(rt.env, rt.make_atom("wxNullCursor"),rt.make_ref(app->getRef((void *)&wxNullCursor,memenv), "wxCursor")),
    enif_make_tuple2(rt.env, rt.make_atom("wxNullFont"),rt.make_ref(app->getRef((void *)&wxNullFont,memenv), "wxFont")),
    enif_make_tuple2(rt.env, rt.make_atom("wxNullIcon"),rt.make_ref(app->getRef((void *)&wxNullIcon,memenv), "wxIcon")),
    enif_make_tuple2(rt.env, rt.make_atom("wxNullPalette"),rt.make_ref(app->getRef((void *)&wxNullPalette,memenv), "wxPalette")),
    enif_make_tuple2(rt.env, rt.make_atom("wxNullPen"),rt.make_ref(app->getRef((void *)&wxNullPen,memenv), "wxPen")),
    enif_make_tuple2(rt.env, rt.make_atom("wxRED"), rt.make(*(wxRED))),
    enif_make_tuple2(rt.env, rt.make_atom("wxRED_BRUSH"),rt.make_ref(app->getRef((void *)wxRED_BRUSH,memenv), "wxBrush")),
    enif_make_tuple2(rt.env, rt.make_atom("wxRED_PEN"),rt.make_ref(app->getRef((void *)wxRED_PEN,memenv), "wxPen")),
    enif_make_tuple2(rt.env, rt.make_atom("wxSMALL_FONT"),rt.make_ref(app->getRef((void *)wxSMALL_FONT,memenv), "wxFont")),
    enif_make_tuple2(rt.env, rt.make_atom("wxSTANDARD_CURSOR"),rt.make_ref(app->getRef((void *)wxSTANDARD_CURSOR,memenv), "wxCursor")),
    enif_make_tuple2(rt.env, rt.make_atom("wxSWISS_FONT"),rt.make_ref(app->getRef((void *)wxSWISS_FONT,memenv), "wxFont")),
    enif_make_tuple2(rt.env, rt.make_atom("wxTRANSPARENT_BRUSH"),rt.make_ref(app->getRef((void *)wxTRANSPARENT_BRUSH,memenv), "wxBrush")),
    enif_make_tuple2(rt.env, rt.make_atom("wxTRANSPARENT_PEN"),rt.make_ref(app->getRef((void *)wxTRANSPARENT_PEN,memenv), "wxPen")),
    enif_make_tuple2(rt.env, rt.make_atom("wxWHITE"), rt.make(*(wxWHITE))),
    enif_make_tuple2(rt.env, rt.make_atom("wxWHITE_BRUSH"),rt.make_ref(app->getRef((void *)wxWHITE_BRUSH,memenv), "wxBrush")),
    enif_make_tuple2(rt.env, rt.make_atom("wxWHITE_PEN"),rt.make_ref(app->getRef((void *)wxWHITE_PEN,memenv), "wxPen")),
#if wxCHECK_VERSION(3,1,0)
    enif_make_tuple2(rt.env, rt.make_atom("wx_GL_COMPAT_PROFILE"), rt.make_int(wx_GL_COMPAT_PROFILE)),
#else
    enif_make_tuple2(rt.env, rt.make_atom("wx_GL_COMPAT_PROFILE"), WXE_ATOM_undefined),
#endif
  };
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
    rt.make_atom("wx_consts"),  enif_make_list_from_array(rt.env, consts, 331));
  rt.send(msg);
}
