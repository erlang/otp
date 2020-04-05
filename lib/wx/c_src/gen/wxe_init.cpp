/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2019. All Rights Reserved.
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
void WxeApp::init_nonconsts(wxeMemEnv *memenv, ErlDrvTermData caller) {
  wxeReturn rt = wxeReturn(WXE_DRV_PORT, caller);
 rt.addAtom((char*)"wx_consts");
 rt.addAtom("wxALWAYS_NATIVE_DOUBLE_BUFFER"); rt.addInt(wxALWAYS_NATIVE_DOUBLE_BUFFER);
 rt.addTupleCount(2);
 rt.addAtom("wxBYTE_ORDER"); rt.addInt(wxBYTE_ORDER);
 rt.addTupleCount(2);
 rt.addAtom("wxDEFAULT_CONTROL_BORDER"); rt.addInt(wxDEFAULT_CONTROL_BORDER);
 rt.addTupleCount(2);
 rt.addAtom("wxHAS_INT64"); rt.addInt(wxHAS_INT64);
 rt.addTupleCount(2);
 rt.addAtom("wxRETAINED"); rt.addInt(wxRETAINED);
 rt.addTupleCount(2);
 rt.addAtom("wxGAUGE_EMULATE_INDETERMINATE_MODE"); rt.addInt(wxGAUGE_EMULATE_INDETERMINATE_MODE);
 rt.addTupleCount(2);
 rt.addAtom("wxSL_LABELS"); rt.addInt(wxSL_LABELS);
 rt.addTupleCount(2);
 rt.addAtom("wxTR_DEFAULT_STYLE"); rt.addInt(wxTR_DEFAULT_STYLE);
 rt.addTupleCount(2);
 rt.addAtom("wxBETA_NUMBER"); rt.addInt(wxBETA_NUMBER);
 rt.addTupleCount(2);
 rt.addAtom("wxMAJOR_VERSION"); rt.addInt(wxMAJOR_VERSION);
 rt.addTupleCount(2);
 rt.addAtom("wxMINOR_VERSION"); rt.addInt(wxMINOR_VERSION);
 rt.addTupleCount(2);
 rt.addAtom("wxRELEASE_NUMBER"); rt.addInt(wxRELEASE_NUMBER);
 rt.addTupleCount(2);
 rt.addAtom("wxSUBRELEASE_NUMBER"); rt.addInt(wxSUBRELEASE_NUMBER);
 rt.addTupleCount(2);
 rt.addAtom("wxIMAGE_QUALITY_HIGH"); rt.addInt(wxIMAGE_QUALITY_HIGH);
 rt.addTupleCount(2);
 rt.addAtom("wxIMAGE_QUALITY_NORMAL"); rt.addInt(wxIMAGE_QUALITY_NORMAL);
 rt.addTupleCount(2);
 rt.addAtom("wxBG_STYLE_COLOUR"); rt.addInt(wxBG_STYLE_COLOUR);
 rt.addTupleCount(2);
 rt.addAtom("wxBG_STYLE_CUSTOM"); rt.addInt(wxBG_STYLE_CUSTOM);
 rt.addTupleCount(2);
 rt.addAtom("wxBG_STYLE_SYSTEM"); rt.addInt(wxBG_STYLE_SYSTEM);
 rt.addTupleCount(2);
 rt.addAtom("wxFONTENCODING_UTF16"); rt.addInt(wxFONTENCODING_UTF16);
 rt.addTupleCount(2);
 rt.addAtom("wxFONTENCODING_UTF32"); rt.addInt(wxFONTENCODING_UTF32);
 rt.addTupleCount(2);
 rt.addAtom("wxFONTWEIGHT_BOLD"); rt.addInt(wxFONTWEIGHT_BOLD);
 rt.addTupleCount(2);
 rt.addAtom("wxFONTWEIGHT_LIGHT"); rt.addInt(wxFONTWEIGHT_LIGHT);
 rt.addTupleCount(2);
 rt.addAtom("wxFONTWEIGHT_MAX"); rt.addInt(wxFONTWEIGHT_MAX);
 rt.addTupleCount(2);
 rt.addAtom("wxFONTWEIGHT_NORMAL"); rt.addInt(wxFONTWEIGHT_NORMAL);
 rt.addTupleCount(2);
 rt.addAtom("wxMOD_CMD"); rt.addInt(wxMOD_CMD);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ABKHAZIAN"); rt.addInt(wxLANGUAGE_ABKHAZIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_AFAR"); rt.addInt(wxLANGUAGE_AFAR);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_AFRIKAANS"); rt.addInt(wxLANGUAGE_AFRIKAANS);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ALBANIAN"); rt.addInt(wxLANGUAGE_ALBANIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_AMHARIC"); rt.addInt(wxLANGUAGE_AMHARIC);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC"); rt.addInt(wxLANGUAGE_ARABIC);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_ALGERIA"); rt.addInt(wxLANGUAGE_ARABIC_ALGERIA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_BAHRAIN"); rt.addInt(wxLANGUAGE_ARABIC_BAHRAIN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_EGYPT"); rt.addInt(wxLANGUAGE_ARABIC_EGYPT);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_IRAQ"); rt.addInt(wxLANGUAGE_ARABIC_IRAQ);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_JORDAN"); rt.addInt(wxLANGUAGE_ARABIC_JORDAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_KUWAIT"); rt.addInt(wxLANGUAGE_ARABIC_KUWAIT);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_LEBANON"); rt.addInt(wxLANGUAGE_ARABIC_LEBANON);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_LIBYA"); rt.addInt(wxLANGUAGE_ARABIC_LIBYA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_MOROCCO"); rt.addInt(wxLANGUAGE_ARABIC_MOROCCO);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_OMAN"); rt.addInt(wxLANGUAGE_ARABIC_OMAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_QATAR"); rt.addInt(wxLANGUAGE_ARABIC_QATAR);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_SAUDI_ARABIA"); rt.addInt(wxLANGUAGE_ARABIC_SAUDI_ARABIA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_SUDAN"); rt.addInt(wxLANGUAGE_ARABIC_SUDAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_SYRIA"); rt.addInt(wxLANGUAGE_ARABIC_SYRIA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_TUNISIA"); rt.addInt(wxLANGUAGE_ARABIC_TUNISIA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_UAE"); rt.addInt(wxLANGUAGE_ARABIC_UAE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARABIC_YEMEN"); rt.addInt(wxLANGUAGE_ARABIC_YEMEN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ARMENIAN"); rt.addInt(wxLANGUAGE_ARMENIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ASSAMESE"); rt.addInt(wxLANGUAGE_ASSAMESE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_AYMARA"); rt.addInt(wxLANGUAGE_AYMARA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_AZERI"); rt.addInt(wxLANGUAGE_AZERI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_AZERI_CYRILLIC"); rt.addInt(wxLANGUAGE_AZERI_CYRILLIC);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_AZERI_LATIN"); rt.addInt(wxLANGUAGE_AZERI_LATIN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_BASHKIR"); rt.addInt(wxLANGUAGE_BASHKIR);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_BASQUE"); rt.addInt(wxLANGUAGE_BASQUE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_BELARUSIAN"); rt.addInt(wxLANGUAGE_BELARUSIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_BENGALI"); rt.addInt(wxLANGUAGE_BENGALI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_BHUTANI"); rt.addInt(wxLANGUAGE_BHUTANI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_BIHARI"); rt.addInt(wxLANGUAGE_BIHARI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_BISLAMA"); rt.addInt(wxLANGUAGE_BISLAMA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_BRETON"); rt.addInt(wxLANGUAGE_BRETON);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_BULGARIAN"); rt.addInt(wxLANGUAGE_BULGARIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_BURMESE"); rt.addInt(wxLANGUAGE_BURMESE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_CAMBODIAN"); rt.addInt(wxLANGUAGE_CAMBODIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_CATALAN"); rt.addInt(wxLANGUAGE_CATALAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_CHINESE"); rt.addInt(wxLANGUAGE_CHINESE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_CHINESE_HONGKONG"); rt.addInt(wxLANGUAGE_CHINESE_HONGKONG);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_CHINESE_MACAU"); rt.addInt(wxLANGUAGE_CHINESE_MACAU);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_CHINESE_SIMPLIFIED"); rt.addInt(wxLANGUAGE_CHINESE_SIMPLIFIED);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_CHINESE_SINGAPORE"); rt.addInt(wxLANGUAGE_CHINESE_SINGAPORE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_CHINESE_TAIWAN"); rt.addInt(wxLANGUAGE_CHINESE_TAIWAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_CHINESE_TRADITIONAL"); rt.addInt(wxLANGUAGE_CHINESE_TRADITIONAL);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_CORSICAN"); rt.addInt(wxLANGUAGE_CORSICAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_CROATIAN"); rt.addInt(wxLANGUAGE_CROATIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_CZECH"); rt.addInt(wxLANGUAGE_CZECH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_DANISH"); rt.addInt(wxLANGUAGE_DANISH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_DEFAULT"); rt.addInt(wxLANGUAGE_DEFAULT);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_DUTCH"); rt.addInt(wxLANGUAGE_DUTCH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_DUTCH_BELGIAN"); rt.addInt(wxLANGUAGE_DUTCH_BELGIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH"); rt.addInt(wxLANGUAGE_ENGLISH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_AUSTRALIA"); rt.addInt(wxLANGUAGE_ENGLISH_AUSTRALIA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_BELIZE"); rt.addInt(wxLANGUAGE_ENGLISH_BELIZE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_BOTSWANA"); rt.addInt(wxLANGUAGE_ENGLISH_BOTSWANA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_CANADA"); rt.addInt(wxLANGUAGE_ENGLISH_CANADA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_CARIBBEAN"); rt.addInt(wxLANGUAGE_ENGLISH_CARIBBEAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_DENMARK"); rt.addInt(wxLANGUAGE_ENGLISH_DENMARK);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_EIRE"); rt.addInt(wxLANGUAGE_ENGLISH_EIRE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_JAMAICA"); rt.addInt(wxLANGUAGE_ENGLISH_JAMAICA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_NEW_ZEALAND"); rt.addInt(wxLANGUAGE_ENGLISH_NEW_ZEALAND);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_PHILIPPINES"); rt.addInt(wxLANGUAGE_ENGLISH_PHILIPPINES);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_SOUTH_AFRICA"); rt.addInt(wxLANGUAGE_ENGLISH_SOUTH_AFRICA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_TRINIDAD"); rt.addInt(wxLANGUAGE_ENGLISH_TRINIDAD);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_UK"); rt.addInt(wxLANGUAGE_ENGLISH_UK);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_US"); rt.addInt(wxLANGUAGE_ENGLISH_US);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ENGLISH_ZIMBABWE"); rt.addInt(wxLANGUAGE_ENGLISH_ZIMBABWE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ESPERANTO"); rt.addInt(wxLANGUAGE_ESPERANTO);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ESTONIAN"); rt.addInt(wxLANGUAGE_ESTONIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_FAEROESE"); rt.addInt(wxLANGUAGE_FAEROESE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_FARSI"); rt.addInt(wxLANGUAGE_FARSI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_FIJI"); rt.addInt(wxLANGUAGE_FIJI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_FINNISH"); rt.addInt(wxLANGUAGE_FINNISH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_FRENCH"); rt.addInt(wxLANGUAGE_FRENCH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_FRENCH_BELGIAN"); rt.addInt(wxLANGUAGE_FRENCH_BELGIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_FRENCH_CANADIAN"); rt.addInt(wxLANGUAGE_FRENCH_CANADIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_FRENCH_LUXEMBOURG"); rt.addInt(wxLANGUAGE_FRENCH_LUXEMBOURG);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_FRENCH_MONACO"); rt.addInt(wxLANGUAGE_FRENCH_MONACO);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_FRENCH_SWISS"); rt.addInt(wxLANGUAGE_FRENCH_SWISS);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_FRISIAN"); rt.addInt(wxLANGUAGE_FRISIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_GALICIAN"); rt.addInt(wxLANGUAGE_GALICIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_GEORGIAN"); rt.addInt(wxLANGUAGE_GEORGIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_GERMAN"); rt.addInt(wxLANGUAGE_GERMAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_GERMAN_AUSTRIAN"); rt.addInt(wxLANGUAGE_GERMAN_AUSTRIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_GERMAN_BELGIUM"); rt.addInt(wxLANGUAGE_GERMAN_BELGIUM);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_GERMAN_LIECHTENSTEIN"); rt.addInt(wxLANGUAGE_GERMAN_LIECHTENSTEIN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_GERMAN_LUXEMBOURG"); rt.addInt(wxLANGUAGE_GERMAN_LUXEMBOURG);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_GERMAN_SWISS"); rt.addInt(wxLANGUAGE_GERMAN_SWISS);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_GREEK"); rt.addInt(wxLANGUAGE_GREEK);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_GREENLANDIC"); rt.addInt(wxLANGUAGE_GREENLANDIC);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_GUARANI"); rt.addInt(wxLANGUAGE_GUARANI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_GUJARATI"); rt.addInt(wxLANGUAGE_GUJARATI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_HAUSA"); rt.addInt(wxLANGUAGE_HAUSA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_HEBREW"); rt.addInt(wxLANGUAGE_HEBREW);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_HINDI"); rt.addInt(wxLANGUAGE_HINDI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_HUNGARIAN"); rt.addInt(wxLANGUAGE_HUNGARIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ICELANDIC"); rt.addInt(wxLANGUAGE_ICELANDIC);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_INDONESIAN"); rt.addInt(wxLANGUAGE_INDONESIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_INTERLINGUA"); rt.addInt(wxLANGUAGE_INTERLINGUA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_INTERLINGUE"); rt.addInt(wxLANGUAGE_INTERLINGUE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_INUKTITUT"); rt.addInt(wxLANGUAGE_INUKTITUT);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_INUPIAK"); rt.addInt(wxLANGUAGE_INUPIAK);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_IRISH"); rt.addInt(wxLANGUAGE_IRISH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ITALIAN"); rt.addInt(wxLANGUAGE_ITALIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ITALIAN_SWISS"); rt.addInt(wxLANGUAGE_ITALIAN_SWISS);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_JAPANESE"); rt.addInt(wxLANGUAGE_JAPANESE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_JAVANESE"); rt.addInt(wxLANGUAGE_JAVANESE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_KANNADA"); rt.addInt(wxLANGUAGE_KANNADA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_KASHMIRI"); rt.addInt(wxLANGUAGE_KASHMIRI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_KASHMIRI_INDIA"); rt.addInt(wxLANGUAGE_KASHMIRI_INDIA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_KAZAKH"); rt.addInt(wxLANGUAGE_KAZAKH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_KERNEWEK"); rt.addInt(wxLANGUAGE_KERNEWEK);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_KINYARWANDA"); rt.addInt(wxLANGUAGE_KINYARWANDA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_KIRGHIZ"); rt.addInt(wxLANGUAGE_KIRGHIZ);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_KIRUNDI"); rt.addInt(wxLANGUAGE_KIRUNDI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_KONKANI"); rt.addInt(wxLANGUAGE_KONKANI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_KOREAN"); rt.addInt(wxLANGUAGE_KOREAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_KURDISH"); rt.addInt(wxLANGUAGE_KURDISH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_LAOTHIAN"); rt.addInt(wxLANGUAGE_LAOTHIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_LATIN"); rt.addInt(wxLANGUAGE_LATIN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_LATVIAN"); rt.addInt(wxLANGUAGE_LATVIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_LINGALA"); rt.addInt(wxLANGUAGE_LINGALA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_LITHUANIAN"); rt.addInt(wxLANGUAGE_LITHUANIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_MACEDONIAN"); rt.addInt(wxLANGUAGE_MACEDONIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_MALAGASY"); rt.addInt(wxLANGUAGE_MALAGASY);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_MALAY"); rt.addInt(wxLANGUAGE_MALAY);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_MALAYALAM"); rt.addInt(wxLANGUAGE_MALAYALAM);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_MALAY_BRUNEI_DARUSSALAM"); rt.addInt(wxLANGUAGE_MALAY_BRUNEI_DARUSSALAM);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_MALAY_MALAYSIA"); rt.addInt(wxLANGUAGE_MALAY_MALAYSIA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_MALTESE"); rt.addInt(wxLANGUAGE_MALTESE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_MANIPURI"); rt.addInt(wxLANGUAGE_MANIPURI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_MAORI"); rt.addInt(wxLANGUAGE_MAORI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_MARATHI"); rt.addInt(wxLANGUAGE_MARATHI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_MOLDAVIAN"); rt.addInt(wxLANGUAGE_MOLDAVIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_MONGOLIAN"); rt.addInt(wxLANGUAGE_MONGOLIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_NAURU"); rt.addInt(wxLANGUAGE_NAURU);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_NEPALI"); rt.addInt(wxLANGUAGE_NEPALI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_NEPALI_INDIA"); rt.addInt(wxLANGUAGE_NEPALI_INDIA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_NORWEGIAN_BOKMAL"); rt.addInt(wxLANGUAGE_NORWEGIAN_BOKMAL);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_NORWEGIAN_NYNORSK"); rt.addInt(wxLANGUAGE_NORWEGIAN_NYNORSK);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_OCCITAN"); rt.addInt(wxLANGUAGE_OCCITAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ORIYA"); rt.addInt(wxLANGUAGE_ORIYA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_OROMO"); rt.addInt(wxLANGUAGE_OROMO);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_PASHTO"); rt.addInt(wxLANGUAGE_PASHTO);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_POLISH"); rt.addInt(wxLANGUAGE_POLISH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_PORTUGUESE"); rt.addInt(wxLANGUAGE_PORTUGUESE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_PORTUGUESE_BRAZILIAN"); rt.addInt(wxLANGUAGE_PORTUGUESE_BRAZILIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_PUNJABI"); rt.addInt(wxLANGUAGE_PUNJABI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_QUECHUA"); rt.addInt(wxLANGUAGE_QUECHUA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_RHAETO_ROMANCE"); rt.addInt(wxLANGUAGE_RHAETO_ROMANCE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ROMANIAN"); rt.addInt(wxLANGUAGE_ROMANIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_RUSSIAN"); rt.addInt(wxLANGUAGE_RUSSIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_RUSSIAN_UKRAINE"); rt.addInt(wxLANGUAGE_RUSSIAN_UKRAINE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SAMI"); rt.addInt(wxLANGUAGE_SAMI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SAMOAN"); rt.addInt(wxLANGUAGE_SAMOAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SANGHO"); rt.addInt(wxLANGUAGE_SANGHO);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SANSKRIT"); rt.addInt(wxLANGUAGE_SANSKRIT);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SCOTS_GAELIC"); rt.addInt(wxLANGUAGE_SCOTS_GAELIC);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SERBIAN"); rt.addInt(wxLANGUAGE_SERBIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SERBIAN_CYRILLIC"); rt.addInt(wxLANGUAGE_SERBIAN_CYRILLIC);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SERBIAN_LATIN"); rt.addInt(wxLANGUAGE_SERBIAN_LATIN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SERBO_CROATIAN"); rt.addInt(wxLANGUAGE_SERBO_CROATIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SESOTHO"); rt.addInt(wxLANGUAGE_SESOTHO);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SETSWANA"); rt.addInt(wxLANGUAGE_SETSWANA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SHONA"); rt.addInt(wxLANGUAGE_SHONA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SINDHI"); rt.addInt(wxLANGUAGE_SINDHI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SINHALESE"); rt.addInt(wxLANGUAGE_SINHALESE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SISWATI"); rt.addInt(wxLANGUAGE_SISWATI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SLOVAK"); rt.addInt(wxLANGUAGE_SLOVAK);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SLOVENIAN"); rt.addInt(wxLANGUAGE_SLOVENIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SOMALI"); rt.addInt(wxLANGUAGE_SOMALI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH"); rt.addInt(wxLANGUAGE_SPANISH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_ARGENTINA"); rt.addInt(wxLANGUAGE_SPANISH_ARGENTINA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_BOLIVIA"); rt.addInt(wxLANGUAGE_SPANISH_BOLIVIA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_CHILE"); rt.addInt(wxLANGUAGE_SPANISH_CHILE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_COLOMBIA"); rt.addInt(wxLANGUAGE_SPANISH_COLOMBIA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_COSTA_RICA"); rt.addInt(wxLANGUAGE_SPANISH_COSTA_RICA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_DOMINICAN_REPUBLIC"); rt.addInt(wxLANGUAGE_SPANISH_DOMINICAN_REPUBLIC);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_ECUADOR"); rt.addInt(wxLANGUAGE_SPANISH_ECUADOR);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_EL_SALVADOR"); rt.addInt(wxLANGUAGE_SPANISH_EL_SALVADOR);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_GUATEMALA"); rt.addInt(wxLANGUAGE_SPANISH_GUATEMALA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_HONDURAS"); rt.addInt(wxLANGUAGE_SPANISH_HONDURAS);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_MEXICAN"); rt.addInt(wxLANGUAGE_SPANISH_MEXICAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_MODERN"); rt.addInt(wxLANGUAGE_SPANISH_MODERN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_NICARAGUA"); rt.addInt(wxLANGUAGE_SPANISH_NICARAGUA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_PANAMA"); rt.addInt(wxLANGUAGE_SPANISH_PANAMA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_PARAGUAY"); rt.addInt(wxLANGUAGE_SPANISH_PARAGUAY);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_PERU"); rt.addInt(wxLANGUAGE_SPANISH_PERU);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_PUERTO_RICO"); rt.addInt(wxLANGUAGE_SPANISH_PUERTO_RICO);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_URUGUAY"); rt.addInt(wxLANGUAGE_SPANISH_URUGUAY);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_US"); rt.addInt(wxLANGUAGE_SPANISH_US);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SPANISH_VENEZUELA"); rt.addInt(wxLANGUAGE_SPANISH_VENEZUELA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SUNDANESE"); rt.addInt(wxLANGUAGE_SUNDANESE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SWAHILI"); rt.addInt(wxLANGUAGE_SWAHILI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SWEDISH"); rt.addInt(wxLANGUAGE_SWEDISH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_SWEDISH_FINLAND"); rt.addInt(wxLANGUAGE_SWEDISH_FINLAND);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_TAGALOG"); rt.addInt(wxLANGUAGE_TAGALOG);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_TAJIK"); rt.addInt(wxLANGUAGE_TAJIK);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_TAMIL"); rt.addInt(wxLANGUAGE_TAMIL);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_TATAR"); rt.addInt(wxLANGUAGE_TATAR);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_TELUGU"); rt.addInt(wxLANGUAGE_TELUGU);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_THAI"); rt.addInt(wxLANGUAGE_THAI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_TIBETAN"); rt.addInt(wxLANGUAGE_TIBETAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_TIGRINYA"); rt.addInt(wxLANGUAGE_TIGRINYA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_TONGA"); rt.addInt(wxLANGUAGE_TONGA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_TSONGA"); rt.addInt(wxLANGUAGE_TSONGA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_TURKISH"); rt.addInt(wxLANGUAGE_TURKISH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_TURKMEN"); rt.addInt(wxLANGUAGE_TURKMEN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_TWI"); rt.addInt(wxLANGUAGE_TWI);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_UIGHUR"); rt.addInt(wxLANGUAGE_UIGHUR);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_UKRAINIAN"); rt.addInt(wxLANGUAGE_UKRAINIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_UNKNOWN"); rt.addInt(wxLANGUAGE_UNKNOWN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_URDU"); rt.addInt(wxLANGUAGE_URDU);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_URDU_INDIA"); rt.addInt(wxLANGUAGE_URDU_INDIA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_URDU_PAKISTAN"); rt.addInt(wxLANGUAGE_URDU_PAKISTAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_USER_DEFINED"); rt.addInt(wxLANGUAGE_USER_DEFINED);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_UZBEK"); rt.addInt(wxLANGUAGE_UZBEK);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_UZBEK_CYRILLIC"); rt.addInt(wxLANGUAGE_UZBEK_CYRILLIC);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_UZBEK_LATIN"); rt.addInt(wxLANGUAGE_UZBEK_LATIN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_VALENCIAN"); rt.addInt(wxLANGUAGE_VALENCIAN);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_VIETNAMESE"); rt.addInt(wxLANGUAGE_VIETNAMESE);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_VOLAPUK"); rt.addInt(wxLANGUAGE_VOLAPUK);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_WELSH"); rt.addInt(wxLANGUAGE_WELSH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_WOLOF"); rt.addInt(wxLANGUAGE_WOLOF);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_XHOSA"); rt.addInt(wxLANGUAGE_XHOSA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_YIDDISH"); rt.addInt(wxLANGUAGE_YIDDISH);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_YORUBA"); rt.addInt(wxLANGUAGE_YORUBA);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ZHUANG"); rt.addInt(wxLANGUAGE_ZHUANG);
 rt.addTupleCount(2);
 rt.addAtom("wxLANGUAGE_ZULU"); rt.addInt(wxLANGUAGE_ZULU);
 rt.addTupleCount(2);
 rt.addAtom("wxCURSOR_ARROWWAIT"); rt.addInt(wxCURSOR_ARROWWAIT);
 rt.addTupleCount(2);
 rt.addAtom("wxCURSOR_DEFAULT"); rt.addInt(wxCURSOR_DEFAULT);
 rt.addTupleCount(2);
 rt.addAtom("wxCURSOR_MAX"); rt.addInt(wxCURSOR_MAX);
 rt.addTupleCount(2);
#if wxCHECK_VERSION(3,0,3)
 rt.addAtom("WX_GL_CORE_PROFILE"); rt.addInt(WX_GL_CORE_PROFILE);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_CORE_PROFILE"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,0)
 rt.addAtom("WX_GL_DEBUG"); rt.addInt(WX_GL_DEBUG);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_DEBUG"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,0)
 rt.addAtom("WX_GL_ES2"); rt.addInt(WX_GL_ES2);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_ES2"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,0)
 rt.addAtom("WX_GL_FORWARD_COMPAT"); rt.addInt(WX_GL_FORWARD_COMPAT);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_FORWARD_COMPAT"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,0)
 rt.addAtom("WX_GL_FRAMEBUFFER_SRGB"); rt.addInt(WX_GL_FRAMEBUFFER_SRGB);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_FRAMEBUFFER_SRGB"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,0)
 rt.addAtom("WX_GL_LOSE_ON_RESET"); rt.addInt(WX_GL_LOSE_ON_RESET);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_LOSE_ON_RESET"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,0,3)
 rt.addAtom("WX_GL_MAJOR_VERSION"); rt.addInt(WX_GL_MAJOR_VERSION);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_MAJOR_VERSION"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,0,3)
 rt.addAtom("WX_GL_MINOR_VERSION"); rt.addInt(WX_GL_MINOR_VERSION);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_MINOR_VERSION"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,0)
 rt.addAtom("WX_GL_NO_RESET_NOTIFY"); rt.addInt(WX_GL_NO_RESET_NOTIFY);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_NO_RESET_NOTIFY"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,0)
 rt.addAtom("WX_GL_RELEASE_FLUSH"); rt.addInt(WX_GL_RELEASE_FLUSH);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_RELEASE_FLUSH"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,0)
 rt.addAtom("WX_GL_RELEASE_NONE"); rt.addInt(WX_GL_RELEASE_NONE);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_RELEASE_NONE"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,0)
 rt.addAtom("WX_GL_RESET_ISOLATION"); rt.addInt(WX_GL_RESET_ISOLATION);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_RESET_ISOLATION"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,0)
 rt.addAtom("WX_GL_ROBUST_ACCESS"); rt.addInt(WX_GL_ROBUST_ACCESS);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_ROBUST_ACCESS"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,0,0)
 rt.addAtom("WX_GL_SAMPLES"); rt.addInt(WX_GL_SAMPLES);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_SAMPLES"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,0,0)
 rt.addAtom("WX_GL_SAMPLE_BUFFERS"); rt.addInt(WX_GL_SAMPLE_BUFFERS);
 rt.addTupleCount(2);
#else
 rt.addAtom("WX_GL_SAMPLE_BUFFERS"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,0,0)
 rt.addAtom("wxBG_STYLE_ERASE"); rt.addInt(wxBG_STYLE_ERASE);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxBG_STYLE_ERASE"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,0,0)
 rt.addAtom("wxBG_STYLE_PAINT"); rt.addInt(wxBG_STYLE_PAINT);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxBG_STYLE_PAINT"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,0,0)
 rt.addAtom("wxBG_STYLE_TRANSPARENT"); rt.addInt(wxBG_STYLE_TRANSPARENT);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxBG_STYLE_TRANSPARENT"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
   rt.addAtom("wxBLACK"); rt.add(*(wxBLACK));
   rt.addTupleCount(2);
   rt.addAtom("wxBLACK_BRUSH"); rt.addRef(getRef((void *)wxBLACK_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxBLACK_DASHED_PEN"); rt.addRef(getRef((void *)wxBLACK_DASHED_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxBLACK_PEN"); rt.addRef(getRef((void *)wxBLACK_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxBLUE"); rt.add(*(wxBLUE));
   rt.addTupleCount(2);
   rt.addAtom("wxBLUE_BRUSH"); rt.addRef(getRef((void *)wxBLUE_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxCROSS_CURSOR"); rt.addRef(getRef((void *)wxCROSS_CURSOR,memenv),"wxCursor");
   rt.addTupleCount(2);
   rt.addAtom("wxCYAN"); rt.add(*(wxCYAN));
   rt.addTupleCount(2);
   rt.addAtom("wxCYAN_BRUSH"); rt.addRef(getRef((void *)wxCYAN_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxCYAN_PEN"); rt.addRef(getRef((void *)wxCYAN_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
#if wxCHECK_VERSION(3,1,2)
 rt.addAtom("wxFONTWEIGHT_EXTRABOLD"); rt.addInt(wxFONTWEIGHT_EXTRABOLD);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxFONTWEIGHT_EXTRABOLD"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,2)
 rt.addAtom("wxFONTWEIGHT_EXTRAHEAVY"); rt.addInt(wxFONTWEIGHT_EXTRAHEAVY);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxFONTWEIGHT_EXTRAHEAVY"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,2)
 rt.addAtom("wxFONTWEIGHT_EXTRALIGHT"); rt.addInt(wxFONTWEIGHT_EXTRALIGHT);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxFONTWEIGHT_EXTRALIGHT"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,2)
 rt.addAtom("wxFONTWEIGHT_HEAVY"); rt.addInt(wxFONTWEIGHT_HEAVY);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxFONTWEIGHT_HEAVY"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,2)
 rt.addAtom("wxFONTWEIGHT_INVALID"); rt.addInt(wxFONTWEIGHT_INVALID);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxFONTWEIGHT_INVALID"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,2)
 rt.addAtom("wxFONTWEIGHT_MEDIUM"); rt.addInt(wxFONTWEIGHT_MEDIUM);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxFONTWEIGHT_MEDIUM"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,2)
 rt.addAtom("wxFONTWEIGHT_SEMIBOLD"); rt.addInt(wxFONTWEIGHT_SEMIBOLD);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxFONTWEIGHT_SEMIBOLD"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,1,2)
 rt.addAtom("wxFONTWEIGHT_THIN"); rt.addInt(wxFONTWEIGHT_THIN);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxFONTWEIGHT_THIN"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
   rt.addAtom("wxGREEN"); rt.add(*(wxGREEN));
   rt.addTupleCount(2);
   rt.addAtom("wxGREEN_BRUSH"); rt.addRef(getRef((void *)wxGREEN_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxGREEN_PEN"); rt.addRef(getRef((void *)wxGREEN_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxGREY_BRUSH"); rt.addRef(getRef((void *)wxGREY_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxGREY_PEN"); rt.addRef(getRef((void *)wxGREY_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxHOURGLASS_CURSOR"); rt.addRef(getRef((void *)wxHOURGLASS_CURSOR,memenv),"wxCursor");
   rt.addTupleCount(2);
#if wxCHECK_VERSION(3,0,0)
 rt.addAtom("wxIMAGE_QUALITY_BICUBIC"); rt.addInt(wxIMAGE_QUALITY_BICUBIC);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxIMAGE_QUALITY_BICUBIC"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,0,0)
 rt.addAtom("wxIMAGE_QUALITY_BILINEAR"); rt.addInt(wxIMAGE_QUALITY_BILINEAR);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxIMAGE_QUALITY_BILINEAR"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,0,0)
 rt.addAtom("wxIMAGE_QUALITY_BOX_AVERAGE"); rt.addInt(wxIMAGE_QUALITY_BOX_AVERAGE);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxIMAGE_QUALITY_BOX_AVERAGE"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
#if wxCHECK_VERSION(3,0,0)
 rt.addAtom("wxIMAGE_QUALITY_NEAREST"); rt.addInt(wxIMAGE_QUALITY_NEAREST);
 rt.addTupleCount(2);
#else
 rt.addAtom("wxIMAGE_QUALITY_NEAREST"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
   rt.addAtom("wxITALIC_FONT"); rt.addRef(getRef((void *)wxITALIC_FONT,memenv),"wxFont");
   rt.addTupleCount(2);
   rt.addAtom("wxLIGHT_GREY"); rt.add(*(wxLIGHT_GREY));
   rt.addTupleCount(2);
   rt.addAtom("wxLIGHT_GREY_BRUSH"); rt.addRef(getRef((void *)wxLIGHT_GREY_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxLIGHT_GREY_PEN"); rt.addRef(getRef((void *)wxLIGHT_GREY_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxMEDIUM_GREY_BRUSH"); rt.addRef(getRef((void *)wxMEDIUM_GREY_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxMEDIUM_GREY_PEN"); rt.addRef(getRef((void *)wxMEDIUM_GREY_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxNORMAL_FONT"); rt.addRef(getRef((void *)wxNORMAL_FONT,memenv),"wxFont");
   rt.addTupleCount(2);
   rt.addAtom("wxNullBitmap"); rt.addRef(getRef((void *)&wxNullBitmap,memenv), "wxBitmap");
   rt.addTupleCount(2);
   rt.addAtom("wxNullBrush"); rt.addRef(getRef((void *)&wxNullBrush,memenv), "wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxNullCursor"); rt.addRef(getRef((void *)&wxNullCursor,memenv), "wxCursor");
   rt.addTupleCount(2);
   rt.addAtom("wxNullFont"); rt.addRef(getRef((void *)&wxNullFont,memenv), "wxFont");
   rt.addTupleCount(2);
   rt.addAtom("wxNullIcon"); rt.addRef(getRef((void *)&wxNullIcon,memenv), "wxIcon");
   rt.addTupleCount(2);
   rt.addAtom("wxNullPalette"); rt.addRef(getRef((void *)&wxNullPalette,memenv), "wxPalette");
   rt.addTupleCount(2);
   rt.addAtom("wxNullPen"); rt.addRef(getRef((void *)&wxNullPen,memenv), "wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxRED"); rt.add(*(wxRED));
   rt.addTupleCount(2);
   rt.addAtom("wxRED_BRUSH"); rt.addRef(getRef((void *)wxRED_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxRED_PEN"); rt.addRef(getRef((void *)wxRED_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxSMALL_FONT"); rt.addRef(getRef((void *)wxSMALL_FONT,memenv),"wxFont");
   rt.addTupleCount(2);
   rt.addAtom("wxSTANDARD_CURSOR"); rt.addRef(getRef((void *)wxSTANDARD_CURSOR,memenv),"wxCursor");
   rt.addTupleCount(2);
   rt.addAtom("wxSWISS_FONT"); rt.addRef(getRef((void *)wxSWISS_FONT,memenv),"wxFont");
   rt.addTupleCount(2);
   rt.addAtom("wxTRANSPARENT_BRUSH"); rt.addRef(getRef((void *)wxTRANSPARENT_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxTRANSPARENT_PEN"); rt.addRef(getRef((void *)wxTRANSPARENT_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxWHITE"); rt.add(*(wxWHITE));
   rt.addTupleCount(2);
   rt.addAtom("wxWHITE_BRUSH"); rt.addRef(getRef((void *)wxWHITE_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxWHITE_PEN"); rt.addRef(getRef((void *)wxWHITE_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
#if wxCHECK_VERSION(3,1,0)
 rt.addAtom("wx_GL_COMPAT_PROFILE"); rt.addInt(wx_GL_COMPAT_PROFILE);
 rt.addTupleCount(2);
#else
 rt.addAtom("wx_GL_COMPAT_PROFILE"); rt.addAtom("undefined");
 rt.addTupleCount(2);
#endif
 rt.endList(333);
 rt.addTupleCount(2);
  rt.send();
}
