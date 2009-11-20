/*************************************************
*      Perl-Compatible Regular Expressions       *
*************************************************/
/* This is a "hacked" version of pcre_maketables that 
 * will generate an acceptable character table for any
 * iso-latin-1 language when running in 8-bit mode.
 */


/* PCRE is a library of functions to support regular expressions whose syntax
and semantics are as close as possible to those of the Perl 5 language.

                       Written by Philip Hazel
           Copyright (c) 1997-2008 University of Cambridge

-----------------------------------------------------------------------------
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

    * Neither the name of the University of Cambridge nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-----------------------------------------------------------------------------
*/

/* %ExternalCopyright% */

/* This module contains the external function pcre_maketables(), which builds
character tables for PCRE in the current locale. The file is compiled on its
own as part of the PCRE library. However, it is also included in the
compilation of dftables.c, in which case the macro DFTABLES is defined. */


#ifndef DFTABLES
#  ifdef HAVE_CONFIG_H
#  include "config.h"
#  endif
#  include "pcre_internal.h"
#endif


/*************************************************
*           Create PCRE character tables         *
*************************************************/

/* This function builds a set of character tables for use by PCRE and returns
a pointer to them. They are build using the ctype functions, and consequently
their contents will depend upon the current locale setting. When compiled as
part of the library, the store is obtained via pcre_malloc(), but when compiled
inside dftables, use malloc().

Arguments:   none
Returns:     pointer to the contiguous block of data
*/

typedef struct {
    int is_alpha,is_upper,is_lower,is_alnum,is_space,is_xdigit,is_graph,is_punct,is_cntrl;
    int upcase;
    int lowcase;
} HiCharProp;

static HiCharProp hicharprop[] = {
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 160       NO-BREAK SPACE */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 161 ¡     INVERTED EXCLAMATION MARK */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 162 ¢     CENT SIGN */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 163 £     POUND SIGN */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 164 ¤     CURRENCY SIGN */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 165 ¥     YEN SIGN */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 166 ¦     BROKEN BAR */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 167 §     SECTION SIGN */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 168 ¨     DIAERESIS */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 169 ©     COPYRIGHT SIGN */
    {1,0,0,1,0,0,1,0,0, 0,0}, /* 170 ª     FEMININE ORDINAL INDICATOR */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 171 «     LEFT-POINTING DOUBLE ANGLE QUOTATION MARK */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 172 ¬     NOT SIGN */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 173 ­     SOFT HYPHEN */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 174 ®     REGISTERED SIGN */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 175 ¯     MACRON */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 176 °     DEGREE SIGN */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 177 ±     PLUS-MINUS SIGN */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 178 ²     SUPERSCRIPT TWO */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 179 ³     SUPERSCRIPT THREE */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 180 ´     ACUTE ACCENT */
    {1,0,1,1,0,0,1,0,0, 0,0}, /* 181 µ     MICRO SIGN */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 182 ¶     PILCROW SIGN */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 183 ·     MIDDLE DOT */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 184 ¸     CEDILLA */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 185 ¹     SUPERSCRIPT ONE */
    {1,0,0,1,0,0,1,0,0, 0,0}, /* 186 º     MASCULINE ORDINAL INDICATOR */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 187 »     RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 188 ¼     VULGAR FRACTION ONE QUARTER */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 189 ½     VULGAR FRACTION ONE HALF */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 190 ¾     VULGAR FRACTION THREE QUARTERS */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 191 ¿     INVERTED QUESTION MARK */
    {1,1,0,1,0,0,1,0,0, 0,224}, /* 192 À     LATIN CAPITAL LETTER A WITH GRAVE */
    {1,1,0,1,0,0,1,0,0, 0,225}, /* 193 Á     LATIN CAPITAL LETTER A WITH ACUTE */
    {1,1,0,1,0,0,1,0,0, 0,226}, /* 194 Â     LATIN CAPITAL LETTER A WITH CIRCUMFLEX */
    {1,1,0,1,0,0,1,0,0, 0,227}, /* 195 Ã     LATIN CAPITAL LETTER A WITH TILDE */
    {1,1,0,1,0,0,1,0,0, 0,228}, /* 196 Ä     LATIN CAPITAL LETTER A WITH DIAERESIS */
    {1,1,0,1,0,0,1,0,0, 0,229}, /* 197 Å     LATIN CAPITAL LETTER A WITH RING ABOVE */
    {1,1,0,1,0,0,1,0,0, 0,230}, /* 198 Æ     LATIN CAPITAL LETTER AE */
    {1,1,0,1,0,0,1,0,0, 0,231}, /* 199 Ç     LATIN CAPITAL LETTER C WITH CEDILLA */
    {1,1,0,1,0,0,1,0,0, 0,232}, /* 200 È     LATIN CAPITAL LETTER E WITH GRAVE */
    {1,1,0,1,0,0,1,0,0, 0,233}, /* 201 É     LATIN CAPITAL LETTER E WITH ACUTE */
    {1,1,0,1,0,0,1,0,0, 0,234}, /* 202 Ê     LATIN CAPITAL LETTER E WITH CIRCUMFLEX */
    {1,1,0,1,0,0,1,0,0, 0,235}, /* 203 Ë     LATIN CAPITAL LETTER E WITH DIAERESIS */
    {1,1,0,1,0,0,1,0,0, 0,236}, /* 204 Ì     LATIN CAPITAL LETTER I WITH GRAVE */
    {1,1,0,1,0,0,1,0,0, 0,237}, /* 205 Í     LATIN CAPITAL LETTER I WITH ACUTE */
    {1,1,0,1,0,0,1,0,0, 0,238}, /* 206 Î     LATIN CAPITAL LETTER I WITH CIRCUMFLEX */
    {1,1,0,1,0,0,1,0,0, 0,239}, /* 207 Ï     LATIN CAPITAL LETTER I WITH DIAERESIS */
    {1,1,0,1,0,0,1,0,0, 0,240}, /* 208 Ð     LATIN CAPITAL LETTER ETH */
    {1,1,0,1,0,0,1,0,0, 0,241}, /* 209 Ñ     LATIN CAPITAL LETTER N WITH TILDE */
    {1,1,0,1,0,0,1,0,0, 0,242}, /* 210 Ò     LATIN CAPITAL LETTER O WITH GRAVE */
    {1,1,0,1,0,0,1,0,0, 0,243}, /* 211 Ó     LATIN CAPITAL LETTER O WITH ACUTE */
    {1,1,0,1,0,0,1,0,0, 0,244}, /* 212 Ô     LATIN CAPITAL LETTER O WITH CIRCUMFLEX */
    {1,1,0,1,0,0,1,0,0, 0,245}, /* 213 Õ     LATIN CAPITAL LETTER O WITH TILDE */
    {1,1,0,1,0,0,1,0,0, 0,246}, /* 214 Ö     LATIN CAPITAL LETTER O WITH DIAERESIS */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 215 ×     MULTIPLICATION SIGN */
    {1,1,0,1,0,0,1,0,0, 0,248}, /* 216 Ø     LATIN CAPITAL LETTER O WITH STROKE */
    {1,1,0,1,0,0,1,0,0, 0,249}, /* 217 Ù     LATIN CAPITAL LETTER U WITH GRAVE */
    {1,1,0,1,0,0,1,0,0, 0,250}, /* 218 Ú     LATIN CAPITAL LETTER U WITH ACUTE */
    {1,1,0,1,0,0,1,0,0, 0,251}, /* 219 Û     LATIN CAPITAL LETTER U WITH CIRCUMFLEX */
    {1,1,0,1,0,0,1,0,0, 0,252}, /* 220 Ü     LATIN CAPITAL LETTER U WITH DIAERESIS */
    {1,1,0,1,0,0,1,0,0, 0,253}, /* 221 Ý     LATIN CAPITAL LETTER Y WITH ACUTE */
    {1,1,0,1,0,0,1,0,0, 0,254}, /* 222 Þ     LATIN CAPITAL LETTER THORN */
    {1,0,1,1,0,0,1,0,0, 223,0}, /* 223 ß     LATIN SMALL LETTER SHARP S Ouch! */
    {1,0,1,1,0,0,1,0,0, 192,0}, /* 224 à     LATIN SMALL LETTER A WITH GRAVE */
    {1,0,1,1,0,0,1,0,0, 193,0}, /* 225 á     LATIN SMALL LETTER A WITH ACUTE */
    {1,0,1,1,0,0,1,0,0, 194,0}, /* 226 â     LATIN SMALL LETTER A WITH CIRCUMFLEX */
    {1,0,1,1,0,0,1,0,0, 195,0}, /* 227 ã     LATIN SMALL LETTER A WITH TILDE */
    {1,0,1,1,0,0,1,0,0, 196,0}, /* 228 ä     LATIN SMALL LETTER A WITH DIAERESIS */
    {1,0,1,1,0,0,1,0,0, 197,0}, /* 229 å     LATIN SMALL LETTER A WITH RING ABOVE */
    {1,0,1,1,0,0,1,0,0, 198,0}, /* 230 æ     LATIN SMALL LETTER AE */
    {1,0,1,1,0,0,1,0,0, 199,0}, /* 231 ç     LATIN SMALL LETTER C WITH CEDILLA */
    {1,0,1,1,0,0,1,0,0, 200,0}, /* 232 è     LATIN SMALL LETTER E WITH GRAVE */
    {1,0,1,1,0,0,1,0,0, 201,0}, /* 233 é     LATIN SMALL LETTER E WITH ACUTE */
    {1,0,1,1,0,0,1,0,0, 202,0}, /* 234 ê     LATIN SMALL LETTER E WITH CIRCUMFLEX */
    {1,0,1,1,0,0,1,0,0, 203,0}, /* 235 ë     LATIN SMALL LETTER E WITH DIAERESIS */
    {1,0,1,1,0,0,1,0,0, 204,0}, /* 236 ì     LATIN SMALL LETTER I WITH GRAVE */
    {1,0,1,1,0,0,1,0,0, 205,0}, /* 237 í     LATIN SMALL LETTER I WITH ACUTE */
    {1,0,1,1,0,0,1,0,0, 206,0}, /* 238 î     LATIN SMALL LETTER I WITH CIRCUMFLEX */
    {1,0,1,1,0,0,1,0,0, 207,0}, /* 239 ï     LATIN SMALL LETTER I WITH DIAERESIS */
    {1,0,1,1,0,0,1,0,0, 208,0}, /* 240 ð     LATIN SMALL LETTER ETH */
    {1,0,1,1,0,0,1,0,0, 209,0}, /* 241 ñ     LATIN SMALL LETTER N WITH TILDE */
    {1,0,1,1,0,0,1,0,0, 210,0}, /* 242 ò     LATIN SMALL LETTER O WITH GRAVE */
    {1,0,1,1,0,0,1,0,0, 211,0}, /* 243 ó     LATIN SMALL LETTER O WITH ACUTE */
    {1,0,1,1,0,0,1,0,0, 212,0}, /* 244 ô     LATIN SMALL LETTER O WITH CIRCUMFLEX */
    {1,0,1,1,0,0,1,0,0, 213,0}, /* 245 õ     LATIN SMALL LETTER O WITH TILDE */
    {1,0,1,1,0,0,1,0,0, 214,0}, /* 246 ö     LATIN SMALL LETTER O WITH DIAERESIS */
    {0,0,0,0,0,0,1,1,0, 0,0}, /* 247 ÷     DIVISION SIGN */
    {1,0,1,1,0,0,1,0,0, 216,0}, /* 248 ø     LATIN SMALL LETTER O WITH STROKE */
    {1,0,1,1,0,0,1,0,0, 217,0}, /* 249 ù     LATIN SMALL LETTER U WITH GRAVE */
    {1,0,1,1,0,0,1,0,0, 218,0}, /* 250 ú     LATIN SMALL LETTER U WITH ACUTE */
    {1,0,1,1,0,0,1,0,0, 219,0}, /* 251 û     LATIN SMALL LETTER U WITH CIRCUMFLEX */
    {1,0,1,1,0,0,1,0,0, 220,0}, /* 252 ü     LATIN SMALL LETTER U WITH DIAERESIS */
    {1,0,1,1,0,0,1,0,0, 221,0}, /* 253 ý     LATIN SMALL LETTER Y WITH ACUTE */
    {1,0,1,1,0,0,1,0,0, 222,0}, /* 254 þ     LATIN SMALL LETTER THORN */
    {1,0,1,1,0,0,1,0,0, 255,0}}; /* 255 ÿ     LATIN SMALL LETTER Y WITH DIAERESIS */


static int my_tolower(int x) {
    if (x < 128)
	return tolower(x);
    else if (x < 160)
	return x;
    else if (hicharprop[x - 160].lowcase == 0) 
	return x;
    else
	return hicharprop[x - 160].lowcase;
}

static int my_toupper(int x) {
    if (x < 128)
	return toupper(x);
    else if (x < 160)
	return x;
    else if (hicharprop[x - 160].upcase == 0) 
	return x;
    else
	return hicharprop[x - 160].upcase;
}

static int my_islower(int x) {
    if (x < 128)
	return islower(x);
    else if (x < 160)
	return 0;
    else
	return hicharprop[x - 160].is_lower;
}
    
static int my_isupper(int x) {
    if (x < 128)
	return isupper(x);
    else if (x < 160)
	return 0;
    else
	return hicharprop[x - 160].is_upper;
}

static int my_isdigit(int x) {
    if (x < 128)
	return isdigit(x);
    else 
	return 0;
}

static int my_isalpha(int x) {
    if (x < 128)
	return isalpha(x);
    else if (x < 160)
	return 0;
    else
	return hicharprop[x - 160].is_alpha;
}

static int my_isalnum(int x) {
    if (x < 128)
	return isalnum(x);
    else if (x < 160)
	return 0;
    else
	return hicharprop[x - 160].is_alnum;
}

static int my_isspace(int x) {
    if (x < 128)
	return isspace(x);
    else if (x < 160)
	return 0;
    else
	return hicharprop[x - 160].is_space;
}

static int my_isxdigit(int x) {
    if (x < 128)
	return isxdigit(x);
    else if (x < 160)
	return 0;
    else
	return hicharprop[x - 160].is_xdigit;
}
static int my_isgraph(int x) {
    if (x < 128)
	return isgraph(x);
    else if (x < 160)
	return 0;
    else
	return hicharprop[x - 160].is_graph;
}
static int my_isprint(int x) {
    if (x < 128)
	return isprint(x);
    else if (x < 160)
	return 0;
    else
	return hicharprop[x - 160].is_graph | hicharprop[x - 160].is_space ;
}

static int my_ispunct(int x) {
    if (x < 128)
	return ispunct(x);
    else if (x < 160)
	return 0;
    else
	return hicharprop[x - 160].is_punct;
}


static int my_iscntrl(int x) {
    if (x < 128)
	return iscntrl(x);
    else if (x < 160)
	return 1;
    else
	return hicharprop[x - 160].is_cntrl;
}
const unsigned char *
pcre_make_latin1_tables(void)
{
unsigned char *yield, *p;
int i;

yield = (unsigned char*)malloc(tables_length);

if (yield == NULL) return NULL;
p = yield;

/* First comes the lower casing table */

for (i = 0; i < 256; i++) *p++ = my_tolower(i);

/* Next the case-flipping table */

for (i = 0; i < 256; i++) *p++ = my_islower(i)? my_toupper(i) : my_tolower(i);

/* Then the character class tables. Don't try to be clever and save effort on
exclusive ones - in some locales things may be different. Note that the table
for "space" includes everything "isspace" gives, including VT in the default
locale. This makes it work for the POSIX class [:space:]. Note also that it is
possible for a character to be alnum or alpha without being lower or upper,
such as "male and female ordinals" (\xAA and \xBA) in the fr_FR locale (at
least under Debian Linux's locales as of 12/2005). So we must test for alnum
specially. */

memset(p, 0, cbit_length);
for (i = 0; i < 256; i++)
  {
  if (my_isdigit(i)) p[cbit_digit  + i/8] |= 1 << (i&7);
  if (my_isupper(i)) p[cbit_upper  + i/8] |= 1 << (i&7);
  if (my_islower(i)) p[cbit_lower  + i/8] |= 1 << (i&7);
  if (my_isalnum(i)) p[cbit_word   + i/8] |= 1 << (i&7);
  if (i == '_')   p[cbit_word   + i/8] |= 1 << (i&7);
  if (my_isspace(i)) p[cbit_space  + i/8] |= 1 << (i&7);
  if (my_isxdigit(i))p[cbit_xdigit + i/8] |= 1 << (i&7);
  if (my_isgraph(i)) p[cbit_graph  + i/8] |= 1 << (i&7);
  if (my_isprint(i)) p[cbit_print  + i/8] |= 1 << (i&7);
  if (my_ispunct(i)) p[cbit_punct  + i/8] |= 1 << (i&7);
  if (my_iscntrl(i)) p[cbit_cntrl  + i/8] |= 1 << (i&7);
  }
p += cbit_length;

/* Finally, the character type table. In this, we exclude VT from the white
space chars, because Perl doesn't recognize it as such for \s and for comments
within regexes. */

for (i = 0; i < 256; i++)
  {
  int x = 0;
  if (i != 0x0b && my_isspace(i)) x += ctype_space;
  if (my_isalpha(i)) x += ctype_letter;
  if (my_isdigit(i)) x += ctype_digit;
  if (my_isxdigit(i)) x += ctype_xdigit;
  if (my_isalnum(i) || i == '_') x += ctype_word;

  /* Note: strchr includes the terminating zero in the characters it considers.
  In this instance, that is ok because we want binary zero to be flagged as a
  meta-character, which in this sense is any character that terminates a run
  of data characters. */

  if (strchr("\\*+?{^.$|()[", i) != 0) x += ctype_meta;
  *p++ = x;
  }

return yield;
}

/* End of pcre_maketables.c */
