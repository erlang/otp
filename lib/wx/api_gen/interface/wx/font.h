/////////////////////////////////////////////////////////////////////////////
// Name:        font.h
// Purpose:     interface of wxFont
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    Standard font families: these are used mainly during wxFont creation to specify
    the generic properties of the font without hardcoding in the sources a specific
    face name.

    wxFontFamily thus allows grouping the font face names of fonts with similar
    properties. Most wxWidgets ports use lists of fonts for each font family
    inspired by the data taken from http://www.codestyle.org/css/font-family.
*/
enum wxFontFamily
{
    wxFONTFAMILY_DEFAULT = wxDEFAULT,           //!< Chooses a default font.

    wxFONTFAMILY_DECORATIVE = wxDECORATIVE,     //!< A decorative font.
    wxFONTFAMILY_ROMAN = wxROMAN,               //!< A formal, serif font.
    wxFONTFAMILY_SCRIPT = wxSCRIPT,             //!< A handwriting font.
    wxFONTFAMILY_SWISS = wxSWISS,               //!< A sans-serif font.

    /// A fixed pitch font. Note that wxFont currently does not make distinctions
    /// between @c wxFONTFAMILY_MODERN and @c wxFONTFAMILY_TELETYPE.
    wxFONTFAMILY_MODERN = wxMODERN,

    /// A teletype (i.e.\ monospaced) font.
    /// Monospace fonts have a fixed width like typewriters and often have strong angular
    /// or block serifs. Monospace font faces are often used code samples and have a simple,
    /// functional font style.
    /// See also wxFont::IsFixedWidth() for an easy way to test for monospace property.
    wxFONTFAMILY_TELETYPE = wxTELETYPE,

    wxFONTFAMILY_MAX,
    /// Invalid font family value, returned by wxFont::GetFamily() when the
    /// font is invalid for example.
    wxFONTFAMILY_UNKNOWN = wxFONTFAMILY_MAX
};

/**
    Font styles.
*/
enum wxFontStyle
{
    /// The font is drawn without slant.
    wxFONTSTYLE_NORMAL = wxNORMAL,

    /// The font is slanted in an italic style.
    wxFONTSTYLE_ITALIC = wxITALIC,

    /// The font is slanted, but in a roman style.
    /// Note that under wxMSW this style is the same as @c wxFONTSTYLE_ITALIC.
    wxFONTSTYLE_SLANT = wxSLANT,

    wxFONTSTYLE_MAX
};

/**
    Font weights.

    The values of this enum correspond to the CSS font weight specifications,
    see https://www.w3.org/TR/css-fonts-4/#font-weight-prop, with the addition of
    one font weight bolder than heavy


*/
enum wxFontWeight
{
    wxFONTWEIGHT_INVALID = 0,        //!< Invalid font weight. @since 3.1.2
    wxFONTWEIGHT_THIN = 100,         //!< Thin font (weight = 100). @since 3.1.2
    wxFONTWEIGHT_EXTRALIGHT = 200,   //!< Extra Light (Ultra Light) font (weight = 200). @since 3.1.2
    wxFONTWEIGHT_LIGHT = 300,        //!< Light font (weight = 300).
    wxFONTWEIGHT_NORMAL = 400,       //!< Normal font (weight = 400).
    wxFONTWEIGHT_MEDIUM = 500,       //!< Medium font (weight = 500). @since 3.1.2
    wxFONTWEIGHT_SEMIBOLD = 600,     //!< Semi Bold (Demi Bold) font (weight = 600). @since 3.1.2
    wxFONTWEIGHT_BOLD = 700,         //!< Bold font (weight = 700).
    wxFONTWEIGHT_EXTRABOLD = 800,    //!< Extra Bold (Ultra Bold) font (weight = 800). @since 3.1.2
    wxFONTWEIGHT_HEAVY = 900,        //!< Heavy (Black) font (weight = 900). @since 3.1.2
    wxFONTWEIGHT_EXTRAHEAVY = 1000,  //!< Extra Heavy font (weight = 1000).  @since 3.1.2
    wxFONTWEIGHT_MAX = wxFONTWEIGHT_EXTRAHEAVY
};

/**
    Symbolic font sizes.

    The elements of this enum correspond to CSS absolute size specifications,
    see http://www.w3.org/TR/CSS21/fonts.html#font-size-props

    @see wxFont::SetSymbolicSize()

    @since 2.9.2
 */
enum wxFontSymbolicSize
{
    wxFONTSIZE_XX_SMALL = -3,   //!< Extra small.
    wxFONTSIZE_X_SMALL,         //!< Very small.
    wxFONTSIZE_SMALL,           //!< Small.
    wxFONTSIZE_MEDIUM,          //!< Normal.
    wxFONTSIZE_LARGE,           //!< Large.
    wxFONTSIZE_X_LARGE,         //!< Very large.
    wxFONTSIZE_XX_LARGE         //!< Extra large.
};

/**
    The font flag bits for the new font ctor accepting one combined flags word.
*/
enum wxFontFlag
{
    /// no special flags: font with default weight/slant/anti-aliasing
    wxFONTFLAG_DEFAULT          = 0,

    /// slant flags (default: no slant)
    wxFONTFLAG_ITALIC           = 1 << 0,
    wxFONTFLAG_SLANT            = 1 << 1,

    /// weight flags (default: medium)
    wxFONTFLAG_LIGHT            = 1 << 2,
    wxFONTFLAG_BOLD             = 1 << 3,

    /// anti-aliasing flag: force on or off (default: the current system default)
    wxFONTFLAG_ANTIALIASED      = 1 << 4,
    wxFONTFLAG_NOT_ANTIALIASED  = 1 << 5,

    /// Underlined style (not underlined by default).
    wxFONTFLAG_UNDERLINED       = 1 << 6,

    /// Strike-through style (implemented in MSW, GTK, and wxOSX)
    wxFONTFLAG_STRIKETHROUGH    = 1 << 7,

    /// the mask of all currently used flags
    wxFONTFLAG_MASK = wxFONTFLAG_ITALIC             |
                      wxFONTFLAG_SLANT              |
                      wxFONTFLAG_LIGHT              |
                      wxFONTFLAG_BOLD               |
                      wxFONTFLAG_ANTIALIASED        |
                      wxFONTFLAG_NOT_ANTIALIASED    |
                      wxFONTFLAG_UNDERLINED         |
                      wxFONTFLAG_STRIKETHROUGH
};



/**
    Font encodings.

    See wxFont::SetEncoding().
*/
enum wxFontEncoding
{
    /// Default system encoding.
    wxFONTENCODING_SYSTEM = -1,     //!< Default system encoding.

    /**
        Default application encoding: this is the encoding set by calls to
        wxFont::SetDefaultEncoding(). Initially, the default application
        encoding is the same as default system encoding.
    */
    wxFONTENCODING_DEFAULT,

    // ISO8859 standard defines a number of single-byte charsets
    wxFONTENCODING_ISO8859_1,       //!< West European (Latin1)
    wxFONTENCODING_ISO8859_2,       //!< Central and East European (Latin2)
    wxFONTENCODING_ISO8859_3,       //!< Esperanto (Latin3)
    wxFONTENCODING_ISO8859_4,       //!< Baltic (old) (Latin4)
    wxFONTENCODING_ISO8859_5,       //!< Cyrillic
    wxFONTENCODING_ISO8859_6,       //!< Arabic
    wxFONTENCODING_ISO8859_7,       //!< Greek
    wxFONTENCODING_ISO8859_8,       //!< Hebrew
    wxFONTENCODING_ISO8859_9,       //!< Turkish (Latin5)
    wxFONTENCODING_ISO8859_10,      //!< Variation of Latin4 (Latin6)
    wxFONTENCODING_ISO8859_11,      //!< Thai
    wxFONTENCODING_ISO8859_12,      //!< doesn't exist currently, but put it
                                    //!< here anyhow to make all ISO8859
                                    //!< consecutive numbers
    wxFONTENCODING_ISO8859_13,      //!< Baltic (Latin7)
    wxFONTENCODING_ISO8859_14,      //!< Latin8
    wxFONTENCODING_ISO8859_15,      //!< Latin9 (a.k.a. Latin0, includes euro)
    wxFONTENCODING_ISO8859_MAX,

    // Cyrillic charset soup (see http://czyborra.com/charsets/cyrillic.html)
    wxFONTENCODING_KOI8,            //!< KOI8 Russian
    wxFONTENCODING_KOI8_U,          //!< KOI8 Ukrainian
    wxFONTENCODING_ALTERNATIVE,     //!< same as MS-DOS CP866
    wxFONTENCODING_BULGARIAN,       //!< used under Linux in Bulgaria

    // what would we do without Microsoft? They have their own encodings
        // for DOS
    wxFONTENCODING_CP437,           //!< original MS-DOS codepage
    wxFONTENCODING_CP850,           //!< CP437 merged with Latin1
    wxFONTENCODING_CP852,           //!< CP437 merged with Latin2
    wxFONTENCODING_CP855,           //!< another cyrillic encoding
    wxFONTENCODING_CP866,           //!< and another one
        // and for Windows
    wxFONTENCODING_CP874,           //!< WinThai
    wxFONTENCODING_CP932,           //!< Japanese (shift-JIS)
    wxFONTENCODING_CP936,           //!< Chinese simplified (GB)
    wxFONTENCODING_CP949,           //!< Korean (Hangul charset)
    wxFONTENCODING_CP950,           //!< Chinese (traditional - Big5)
    wxFONTENCODING_CP1250,          //!< WinLatin2
    wxFONTENCODING_CP1251,          //!< WinCyrillic
    wxFONTENCODING_CP1252,          //!< WinLatin1
    wxFONTENCODING_CP1253,          //!< WinGreek (8859-7)
    wxFONTENCODING_CP1254,          //!< WinTurkish
    wxFONTENCODING_CP1255,          //!< WinHebrew
    wxFONTENCODING_CP1256,          //!< WinArabic
    wxFONTENCODING_CP1257,          //!< WinBaltic (same as Latin 7)
    wxFONTENCODING_CP1258,          //!< WinVietnamese (since 2.9.4)
    wxFONTENCODING_CP1361,          //!< Johab Korean character set (since 2.9.4)
    wxFONTENCODING_CP12_MAX,

    wxFONTENCODING_UTF7,            //!< UTF-7 Unicode encoding
    wxFONTENCODING_UTF8,            //!< UTF-8 Unicode encoding
    wxFONTENCODING_EUC_JP,          //!< Extended Unix Codepage for Japanese
    wxFONTENCODING_UTF16BE,         //!< UTF-16 Big Endian Unicode encoding
    wxFONTENCODING_UTF16LE,         //!< UTF-16 Little Endian Unicode encoding
    wxFONTENCODING_UTF32BE,         //!< UTF-32 Big Endian Unicode encoding
    wxFONTENCODING_UTF32LE,         // UTF-32 Little Endian Unicode encoding

    wxFONTENCODING_MACROMAN,        //!< the standard mac encodings
    wxFONTENCODING_MACJAPANESE,
    wxFONTENCODING_MACCHINESETRAD,
    wxFONTENCODING_MACKOREAN,
    wxFONTENCODING_MACARABIC,
    wxFONTENCODING_MACHEBREW,
    wxFONTENCODING_MACGREEK,
    wxFONTENCODING_MACCYRILLIC,
    wxFONTENCODING_MACDEVANAGARI,
    wxFONTENCODING_MACGURMUKHI,
    wxFONTENCODING_MACGUJARATI,
    wxFONTENCODING_MACORIYA,
    wxFONTENCODING_MACBENGALI,
    wxFONTENCODING_MACTAMIL,
    wxFONTENCODING_MACTELUGU,
    wxFONTENCODING_MACKANNADA,
    wxFONTENCODING_MACMALAJALAM,
    wxFONTENCODING_MACSINHALESE,
    wxFONTENCODING_MACBURMESE,
    wxFONTENCODING_MACKHMER,
    wxFONTENCODING_MACTHAI,
    wxFONTENCODING_MACLAOTIAN,
    wxFONTENCODING_MACGEORGIAN,
    wxFONTENCODING_MACARMENIAN,
    wxFONTENCODING_MACCHINESESIMP,
    wxFONTENCODING_MACTIBETAN,
    wxFONTENCODING_MACMONGOLIAN,
    wxFONTENCODING_MACETHIOPIC,
    wxFONTENCODING_MACCENTRALEUR,
    wxFONTENCODING_MACVIATNAMESE,
    wxFONTENCODING_MACARABICEXT,
    wxFONTENCODING_MACSYMBOL,
    wxFONTENCODING_MACDINGBATS,
    wxFONTENCODING_MACTURKISH,
    wxFONTENCODING_MACCROATIAN,
    wxFONTENCODING_MACICELANDIC,
    wxFONTENCODING_MACROMANIAN,
    wxFONTENCODING_MACCELTIC,
    wxFONTENCODING_MACGAELIC,
    wxFONTENCODING_MACKEYBOARD,

    // more CJK encodings (for historical reasons some are already declared
    // above)
    wxFONTENCODING_ISO2022_JP,      //!< ISO-2022-JP JIS encoding

    wxFONTENCODING_MAX,             //!< highest enumerated encoding value

    wxFONTENCODING_MACMIN = wxFONTENCODING_MACROMAN ,
    wxFONTENCODING_MACMAX = wxFONTENCODING_MACKEYBOARD ,

    // aliases for endian-dependent UTF encodings
    wxFONTENCODING_UTF16,  //!< native UTF-16
    wxFONTENCODING_UTF32,  //!< native UTF-32

    /// Alias for the native Unicode encoding on this platform
    /// (this is used by wxEncodingConverter and wxUTFFile only for now)
    wxFONTENCODING_UNICODE,

    wxFONTENCODING_GB2312 = wxFONTENCODING_CP936, //!< Simplified Chinese
    wxFONTENCODING_BIG5 = wxFONTENCODING_CP950,   //!< Traditional Chinese
    wxFONTENCODING_SHIFT_JIS = wxFONTENCODING_CP932, //!< Shift JIS
    wxFONTENCODING_EUC_KR = wxFONTENCODING_CP949, //!< Korean
    wxFONTENCODING_JOHAB = wxFONTENCODING_CP1361, //!< Korean Johab (since 2.9.4)
    wxFONTENCODING_VIETNAMESE = wxFONTENCODING_CP1258 //!< Vietnamese (since 2.9.4)
};


/**
    @class wxFontInfo

    This class is a helper used for wxFont creation using named parameter
    idiom: it allows specifying various wxFont attributes using the chained
    calls to its clearly named methods instead of passing them in the fixed
    order to wxFont constructors.

    For example, to create an italic font with the given face name and size you
    could use:
    @code
        wxFont font(wxFontInfo(12).FaceName("Helvetica").Italic());
    @endcode

    Notice that all of the methods of this object return a reference to the
    object itself, allowing the calls to them to be chained as in the example
    above.

    All methods taking boolean parameters can be used to turn the specified
    font attribute on or off and turn it on by default.

    @since 2.9.5
 */
class wxFontInfo
{
public:
    /**
        Default constructor uses the default font size for the current
        platform.
     */
    wxFontInfo();

    /**
        Constructor setting the font size in points to use.

        Note that until wxWidgets 3.1.2 fractional point sizes were not
        supported, and the type of @a pointSize was @c int.

        @see wxFont::SetPointSize()
     */
    explicit wxFontInfo(double pointSize);

    /**
        Constructor setting the font size in pixels to use.

        @see wxFont::SetPixelSize()
     */
    explicit wxFontInfo(const wxSize& pixelSize);

    /**
        Set the font family.

        The family is a generic portable way of referring to fonts without
        specifying a precise face name. This parameter must be one of the
        ::wxFontFamily enumeration values.

        If the FaceName() is used, then it overrides the font family.

        @see wxFont::SetFamily()
     */
    wxFontInfo& Family(wxFontFamily family);

    /**
        Set the font face name to use.

        Face names are not portable, so prefer to use Family() in portable
        code.

        @see wxFont::SetFaceName()
     */
    wxFontInfo& FaceName(const wxString& faceName);

    /**
        Specify the weight of the font.

        @param weight
            A font weight in the range from 1 to 1000, inclusive, with 1 being
            the thinnest and 1000 the heaviest possible font variant.
            @c wxFONTWEIGHT_XXX values from wxFontWeight enum can be used here.

        @since 3.1.2
     */
    wxFontInfo& Weight(int weight);

    /**
        Use a bold version of the font.

        This is a wrapper for Weight() calling it with ::wxFONTWEIGHT_BOLD
        argument.

        @see ::wxFontWeight, wxFont::SetWeight()
     */
    wxFontInfo& Bold(bool bold = true);

    /**
        Use a lighter version of the font.

        This is a wrapper for Weight() calling it with ::wxFONTWEIGHT_LIGHT
        argument.

        @see ::wxFontWeight, wxFont::SetWeight()
     */
    wxFontInfo& Light(bool light = true);

    /**
        Use an italic version of the font.

        This is a wrapper for Style() calling it with ::wxFONTSTYLE_ITALIC
        argument.

        @see ::wxFontStyle, wxFont::SetStyle()
     */
    wxFontInfo& Italic(bool italic = true);

    /**
        Use a slanted version of the font.

        This is a wrapper for Style() calling it with ::wxFONTSTYLE_SLANT
        argument.

        @see ::wxFontStyle, wxFont::SetStyle()
     */
    wxFontInfo& Slant(bool slant = true);

    /**
        Specify the style of the font using one of wxFontStyle constants.

        @since 3.1.2
     */
    wxFontInfo& Style(wxFontStyle style);

    /**
        Set anti-aliasing flag.

        Force the use of anti-aliasing on or off.

        Currently this is not implemented, i.e. using this method doesn't do
        anything.
     */
    wxFontInfo& AntiAliased(bool antiAliased = true);

    /**
        Use an underlined version of the font.
     */
    wxFontInfo& Underlined(bool underlined = true);

    /**
        Use a strike-through version of the font.

        Currently this is only implemented in wxMSW, wxGTK, and wxOSX.
     */
    wxFontInfo& Strikethrough(bool strikethrough = true);

    /**
        Set the font encoding to use.

        This is mostly unneeded in Unicode builds of wxWidgets.

        @see ::wxFontEncoding, wxFont::SetEncoding()
     */
    wxFontInfo& Encoding(wxFontEncoding encoding);

    /**
        Set all the font attributes at once.

        See ::wxFontFlag for the various flags that can be used.

        Note that calling this method affects the font weight stored in this
        object: it is set to ::wxFONTWEIGHT_LIGHT or ::wxFONTWEIGHT_BOLD if the
        corresponding flag is present in @a flags, or ::wxFONTWEIGHT_NORMAL
        otherwise.
     */
    wxFontInfo& AllFlags(int flags);

    /**
        Get the symbolic weight closest to the given raw weight value.

        @param numWeight
            A valid raw weight value, i.e. a value in the range 1 to 1000,
            inclusive.
        @return A valid element of wxFontWeight enum.

        @since 3.1.2
     */
    static wxFontWeight GetWeightClosestToNumericValue(int numWeight);
};

/**
    @class wxFont

    A font is an object which determines the appearance of text.

    Fonts are used for drawing text to a device context, and setting the appearance
    of a window's text, see wxDC::SetFont() and wxWindow::SetFont().

    The easiest way to create a custom font is to use wxFontInfo object to
    specify the font attributes and then use wxFont::wxFont(const wxFontInfo&)
    constructor. Alternatively, you could start with one of the pre-defined
    fonts or use wxWindow::GetFont() and modify the font, e.g. by increasing
    its size using MakeLarger() or changing its weight using MakeBold().

    This class uses @ref overview_refcount "reference counting and copy-on-write"
    internally so that assignments between two instances of this class are very
    cheap. You can therefore use actual objects instead of pointers without
    efficiency problems. If an instance of this class is changed it will create
    its own data internally so that other instances, which previously shared the
    data using the reference counting, are not affected.

    You can retrieve the current system font settings with wxSystemSettings.

    @library{wxcore}
    @category{gdi}

    @stdobjects
    ::wxNullFont, ::wxNORMAL_FONT, ::wxSMALL_FONT, ::wxITALIC_FONT, ::wxSWISS_FONT

    @see @ref overview_font, wxDC::SetFont, wxDC::DrawText,
         wxDC::GetTextExtent, wxFontDialog, wxSystemSettings
*/
class wxFont : public wxGDIObject
{
public:
    /**
        Default ctor.
    */
    wxFont();

    /**
        Copy constructor, uses @ref overview_refcount "reference counting".
    */
    wxFont(const wxFont& font);

    /**
        Creates a font object using the specified font description.

        This is the preferred way to create font objects as using this ctor
        results in more readable code and it is also extensible, e.g. it could
        continue to be used if support for more font attributes is added in the
        future. For example, this constructor provides the only way of creating
        fonts with strike-through style.

        Example of creating a font using this ctor:
        @code
            wxFont font(wxFontInfo(10).Bold().Underlined());
        @endcode

        @since 2.9.5
     */
    wxFont(const wxFontInfo& fontInfo);

    /**
        Creates a font object with the specified attributes and size in points.

        Notice that the use of this constructor is often more verbose and less
        readable than using wxFont(const wxFontInfo& font), e.g. the example
        in that constructor documentation would need to be written as:

        @code
            wxFont font(10, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL,
                        wxFONTWEIGHT_BOLD, true);
        @endcode

        @param pointSize
            Size in points. See SetPointSize() for more info. Notice that, for
            historical reasons, the value 70 here is interpreted at @c
            wxDEFAULT and results in creation of the font with the default size
            and not of a font with the size of 70pt. If you really need the
            latter, please use SetPointSize(70). Note that this constructor and
            the matching Create() method overload are the only places in wxFont
            API handling @c wxDEFAULT specially: neither SetPointSize() nor the
            constructor taking wxFontInfo handle this value in this way.
        @param family
            The font family: a generic portable way of referring to fonts without specifying a
            facename. This parameter must be one of the ::wxFontFamily enumeration values.
            If the @a faceName argument is provided, then it overrides the font family.
        @param style
            One of @c wxFONTSTYLE_NORMAL, @c wxFONTSTYLE_SLANT and @c wxFONTSTYLE_ITALIC.
        @param weight
            Font weight, sometimes also referred to as font boldness.
            One of the ::wxFontWeight enumeration values.
        @param underline
            The value can be @true or @false.
            At present this has an effect on Windows and Motif 2.x only.
        @param faceName
            An optional string specifying the face name to be used.
            If it is an empty string, a default face name will be chosen based on the family.
        @param encoding
            An encoding which may be one of the enumeration values of
            ::wxFontEncoding. If the specified encoding isn't available, no font
            is created (see also @ref overview_fontencoding).

        @remarks If the desired font does not exist, the closest match will be
                 chosen. Under Windows, only scalable TrueType fonts are used.
    */
    wxFont(int pointSize, wxFontFamily family, wxFontStyle style,
           wxFontWeight weight,
           bool underline = false,
           const wxString& faceName = wxEmptyString,
           wxFontEncoding encoding = wxFONTENCODING_DEFAULT);

    /**
        Creates a font object with the specified attributes and size in pixels.

        Notice that the use of this constructor is often more verbose and less
        readable than the use of constructor from wxFontInfo, consider using
        that constructor instead.

        @param pixelSize
            Size in pixels. See SetPixelSize() for more info.
        @param family
            The font family: a generic portable way of referring to fonts without specifying a
            facename. This parameter must be one of the ::wxFontFamily enumeration values.
            If the @a faceName argument is provided, then it overrides the font family.
        @param style
            One of @c wxFONTSTYLE_NORMAL, @c wxFONTSTYLE_SLANT and @c wxFONTSTYLE_ITALIC.
        @param weight
            Font weight, sometimes also referred to as font boldness.
            One of the ::wxFontWeight enumeration values.
        @param underline
            The value can be @true or @false.
            At present this has an effect on Windows and Motif 2.x only.
        @param faceName
            An optional string specifying the face name to be used.
            If it is an empty string, a default face name will be chosen based on the family.
        @param encoding
            An encoding which may be one of the enumeration values of
            ::wxFontEncoding. If the specified encoding isn't available, no font
            is created (see also @ref overview_fontencoding).

        @remarks If the desired font does not exist, the closest match will be
                 chosen. Under Windows, only scalable TrueType fonts are used.
    */
    wxFont(const wxSize& pixelSize, wxFontFamily family,
           wxFontStyle style, wxFontWeight weight,
           bool underline = false,
           const wxString& faceName = wxEmptyString,
           wxFontEncoding encoding = wxFONTENCODING_DEFAULT);

    /**
        Constructor from font description string.

        This constructor uses SetNativeFontInfo() to initialize the font.
        If @a fontdesc is invalid the font remains uninitialized, i.e. its IsOk() method
        will return @false.
     */
    wxFont(const wxString& nativeInfoString);

    /**
       Construct font from a native font info structure.
    */
    wxFont(const wxNativeFontInfo& nativeInfo);

    /**
        Destructor.

        See @ref overview_refcount_destruct "reference-counted object destruction"
        for more info.

        @remarks Although all remaining fonts are deleted when the application
                 exits, the application should try to clean up all fonts
                 itself. This is because wxWidgets cannot know if a
                 pointer to the font object is stored in an application
                 data structure, and there is a risk of double deletion.
    */
    virtual ~wxFont();


    /**
        @name Getters
    */
    //@{

    /**
       Returns a font with the same face/size as the given one but with normal
       weight and style and not underlined nor stricken through.

       @since 3.1.0
    */
    wxFont GetBaseFont() const;

    /**
        Returns the encoding of this font.

        Note that under wxGTK the returned value is always @c wxFONTENCODING_UTF8.

        @see SetEncoding()
    */
    virtual wxFontEncoding GetEncoding() const;

    /**
        Returns the face name associated with the font, or the empty string if
        there is no face information.

        @see SetFaceName()
    */
    virtual wxString GetFaceName() const;

    /**
        Gets the font family if possible.

        As described in ::wxFontFamily docs the returned value acts as a rough,
        basic classification of the main font properties (look, spacing).

        If the current font face name is not recognized by wxFont or by the
        underlying system, @c wxFONTFAMILY_DEFAULT is returned.

        Note that currently this function is not very precise and so not
        particularly useful. Font families mostly make sense only for font
        creation, see SetFamily().

        @see SetFamily()
    */
    virtual wxFontFamily GetFamily() const;

    /**
        Returns the platform-dependent string completely describing this font.

        Returned string is always non-empty unless the font is invalid (in
        which case an assert is triggered).

        Note that the returned string is not meant to be shown or edited by the user: a
        typical use of this function is for serializing in string-form a wxFont object.

        @see SetNativeFontInfo(), GetNativeFontInfoUserDesc()
    */
    wxString GetNativeFontInfoDesc() const;

    /**
        Returns a user-friendly string for this font object.

        Returned string is always non-empty unless the font is invalid (in
        which case an assert is triggered).

        The string does not encode all wxFont infos under all platforms;
        e.g. under wxMSW the font family is not present in the returned string.

        Some examples of the formats of returned strings (which are platform-dependent)
        are in SetNativeFontInfoUserDesc().

        @see SetNativeFontInfoUserDesc(), GetNativeFontInfoDesc()
    */
    wxString GetNativeFontInfoUserDesc() const;

    const wxNativeFontInfo *GetNativeFontInfo() const;

    /**
        Specify the name of a file containing a TrueType font to be
        made available to the current application.

        This method can be used to allow this application to use the font from
        the given file even if it is not globally installed on the system.

        Under macOS this method actually doesn't do anything other than check
        for the existence of the file in the "Fonts" subdirectory of the
        application bundle "Resources" directory. You are responsible for
        actually making the font file available in this directory and setting
        @c ATSApplicationFontsPath to @c Fonts value in your @c Info.plist
        file. See also wxStandardPaths::GetResourcesDir().

        Under MSW this method must be called before any wxGraphicsContext
        objects have been created, otherwise the private font won't be usable
        from them.

        Under Unix this method requires Pango 1.38 or later and will return @a
        false and log an error message explaining the problem if this
        requirement is not satisfied either at compile- or run-time.

        Currently this method is implemented for all major platforms (subject
        to having Pango 1.38 or later when running configure under Unix) and
        @c wxUSE_PRIVATE_FONTS is always set to 0 under the other platforms,
        making this function unavailable at compile-time.

        @return @true if the font was added and can now be used.

        @since 3.1.1
    */
    static bool AddPrivateFont(const wxString& filename);

    /**
        Gets the point size as an integer number.

        This function is kept for compatibility reasons. New code should use
        GetFractionalPointSize() and support fractional point sizes.

        @see SetPointSize(), @see GetFractionalPointSize()
    */
    virtual int GetPointSize() const;

    /**
        Gets the point size as a floating number.

        @see SetPointSize(float)

        @since 3.1.2
    */
    virtual double GetFractionalPointSize() const;

    /**
        Gets the pixel size.

        Note that under wxMSW if you passed to SetPixelSize() (or to the ctor)
        a wxSize object with a null width value, you'll get a null width in
        the returned object.

        @see SetPixelSize()
    */
    virtual wxSize GetPixelSize() const;

    /**
        Gets the font style. See ::wxFontStyle for a list of valid styles.

        @see SetStyle()
    */
    virtual wxFontStyle GetStyle() const;

    /**
        Returns @true if the font is underlined, @false otherwise.

        @see SetUnderlined()
    */
    virtual bool GetUnderlined() const;

    /**
        Returns @true if the font is stricken-through, @false otherwise.

        @see SetStrikethrough()

        @since 2.9.4
     */
    virtual bool GetStrikethrough() const;

    /**
        Gets the font weight. See ::wxFontWeight for a list of valid weight identifiers.

        @see SetWeight()
    */
    virtual wxFontWeight GetWeight() const;

    /**
        Gets the font weight as an integer value.

        See ::wxFontWeight for a list of valid weight identifiers and their corresponding integer value.

        @see SetWeight()
        @see SetNumericWeight()

        @since 3.1.2
    */
    virtual int GetNumericWeight() const;

    /**
        Returns @true if the font is a fixed width (or monospaced) font,
        @false if it is a proportional one or font is invalid.

        Note that this function under some platforms is different from just testing
        for the font family being equal to @c wxFONTFAMILY_TELETYPE because native
        platform-specific functions are used for the check (resulting in a more
        accurate return value).
    */
    virtual bool IsFixedWidth() const;

    /**
        Returns @true if this object is a valid font, @false otherwise.
    */
    virtual bool IsOk() const;

    //@}


    /**
        @name Similar fonts creation

        The functions in this section either modify the font in place or create
        a new font similar to the given one but with its weight, style or size
        changed.
     */
    //@{

    /**
        Returns a bold version of this font.

        @see MakeBold()

        @since 2.9.1
     */
    wxFont Bold() const;

    /**
        Returns an italic version of this font.

        @see MakeItalic()

        @since 2.9.1
     */
    wxFont Italic() const;

    /**
        Returns a larger version of this font.

        The font size is multiplied by @c 1.2, the factor of @c 1.2 being
        inspired by the W3C CSS specification.

        @see MakeLarger(), Smaller(), Scaled()

        @since 2.9.1
     */
    wxFont Larger() const;

    /**
        Returns a smaller version of this font.

        The font size is divided by @c 1.2, the factor of @c 1.2 being
        inspired by the W3C CSS specification.

        @see MakeSmaller(), Larger(), Scaled()

        @since 2.9.1
     */
    wxFont Smaller() const;

    /**
        Returns underlined version of this font.

        @see MakeUnderlined()

        @since 2.9.2
     */
    wxFont Underlined() const;

    /**
        Returns stricken-through version of this font.

        Currently stricken-through fonts are only supported in wxMSW, wxGTK, and wxOSX.

        @see MakeStrikethrough()

        @since 2.9.4
     */
    wxFont Strikethrough() const;

    /**
        Changes this font to be bold.

        @see Bold()

        @since 2.9.1
     */
    wxFont& MakeBold();

    /**
        Changes this font to be italic.

        @see Italic()

        @since 2.9.1
     */
    wxFont& MakeItalic();

    /**
        Changes this font to be larger.

        The font size is multiplied by @c 1.2, the factor of @c 1.2 being
        inspired by the W3C CSS specification.

        @see Larger(), MakeSmaller(), Scale()

        @since 2.9.1
     */
    wxFont& MakeLarger();

    /**
        Changes this font to be smaller.

        The font size is divided by @c 1.2, the factor of @c 1.2 being
        inspired by the W3C CSS specification.

        @see Smaller(), MakeLarger(), Scale()

        @since 2.9.1
     */
    wxFont& MakeSmaller();

    /**
        Changes this font to be underlined.

        @see Underlined()

        @since 2.9.2
     */
    wxFont& MakeUnderlined();

    /**
        Changes this font to be stricken-through.

        Currently stricken-through fonts are only supported in wxMSW, wxGTK, and wxOSX.

        @see Strikethrough()

        @since 2.9.4
    */
    wxFont& MakeStrikethrough();

    /**
        Changes the size of this font.

        The font size is multiplied by the given factor (which may be less than
        1 to create a smaller version of the font).

        @see Scaled(), MakeLarger(), MakeSmaller()

        @since 2.9.1
     */
    wxFont& Scale(float x);

    /**
        Returns a scaled version of this font.

        The font size is multiplied by the given factor (which may be less than
        1 to create a smaller version of the font).

        @see Scale(), Larger(), Smaller()

        @since 2.9.1
     */
    wxFont Scaled(float x) const;

    //@}

    /**
        @name Setters

        These functions internally recreate the native font object with the new
        specified property.
    */
    //@{

    /**
        Sets the encoding for this font.

        Note that under wxGTK this function has no effect (because the underlying
        Pango library always uses @c wxFONTENCODING_UTF8).

        @see GetEncoding()
    */
    virtual void SetEncoding(wxFontEncoding encoding);

    /**
        Sets the facename for the font.

        @param faceName
            A valid facename, which should be on the end-user's system.

        @remarks To avoid portability problems, don't rely on a specific face,
                 but specify the font family instead (see ::wxFontFamily and SetFamily()).

        @return @true if the given face name exists; if the face name doesn't exist
                in the user's system then the font is invalidated (so that IsOk() will
                return @false) and @false is returned.

        @see GetFaceName(), SetFamily()
    */
    virtual bool SetFaceName(const wxString& faceName);

    /**
        Sets the font family.

        As described in ::wxFontFamily docs the given @a family value acts as a rough,
        basic indication of the main font properties (look, spacing).

        Note that changing the font family results in changing the font face name.

        @param family
            One of the ::wxFontFamily values.

        @see GetFamily(), SetFaceName()
    */
    virtual void SetFamily(wxFontFamily family);

    /**
        Creates the font corresponding to the given native font description string
        which must have been previously returned by GetNativeFontInfoDesc().

        If the string is invalid, font is unchanged.
        This function is typically used for de-serializing a wxFont object
        previously saved in a string-form.

        @return @true if the creation was successful.

        @see SetNativeFontInfoUserDesc()
    */
    bool SetNativeFontInfo(const wxString& info);

    /**
        Creates the font corresponding to the given native font description string and
        returns @true if the creation was successful.

        Unlike SetNativeFontInfo(), this function accepts strings which are user-friendly.
        Examples of accepted string formats are:

        @beginTable
        @hdr3col{platform, generic syntax, example}
        @row3col{wxGTK2, <tt>[underlined] [strikethrough] [FACE-NAME] [bold] [oblique|italic] [POINTSIZE]</tt>, Monospace bold 10}
        @row3col{wxMSW, <tt>[light|bold] [italic] [FACE-NAME] [POINTSIZE] [ENCODING]</tt>, Tahoma 10 WINDOWS-1252}
        @endTable

        @todo add an example for wxMac

        For more detailed information about the allowed syntaxes you can look at the
        documentation of the native API used for font-rendering
        (e.g. @c pango_font_description_from_string under GTK, although notice
        that it doesn't support the "underlined" and "strikethrough" attributes
        and so those are handled by wxWidgets itself).

        Note that unlike SetNativeFontInfo(), this function doesn't always restore all
        attributes of the wxFont object under all platforms; e.g. on wxMSW the font family
        is not restored (because GetNativeFontInfoUserDesc doesn't return it on wxMSW).
        If you want to serialize/deserialize a font in string form, you should use
        GetNativeFontInfoDesc() and SetNativeFontInfo() instead.

        @see SetNativeFontInfo()
    */
    bool SetNativeFontInfoUserDesc(const wxString& info);

    void SetNativeFontInfo(const wxNativeFontInfo& info);

    /**
        Sets the font size in points to an integer value.

        This is a legacy version of the function only supporting integer point
        sizes. It can still be used, but to avoid unnecessarily restricting the
        font size in points to integer values, consider using the new (added in
        wxWidgets 3.1.2) SetFractionalPointSize() function instead.
     */
    virtual void SetPointSize(int pointSize);

    /**
        Sets the font size in points.

        The <em>point size</em> is defined as 1/72 of the Anglo-Saxon inch
        (25.4 mm): it is approximately 0.0139 inch or 352.8 um.

        @param pointSize
            Size in points. This can also be a fractional point size like 11.5.

        @see GetFractionalPointSize(), SetPointSize()

        @since 3.1.2
    */
    virtual void SetFractionalPointSize(double pointSize);

    /**
        Sets the pixel size.

        The height parameter of @a pixelSize must be positive while the width
        parameter may also be zero (to indicate that you're not interested in the
        width of the characters: a suitable width will be chosen for best rendering).

        This feature (specifying the font pixel size) is directly supported only
        under wxMSW and wxGTK currently; under other platforms a font with the
        closest size to the given one is found using binary search (this maybe slower).

        @see GetPixelSize()
    */
    virtual void SetPixelSize(const wxSize& pixelSize);

    /**
        Sets the font style.

        @param style
            One of the ::wxFontStyle enumeration values.

        @see GetStyle()
    */
    virtual void SetStyle(wxFontStyle style);

    /**
        Sets the font size using a predefined symbolic size name.

        This function allows changing font size to be (very) large or small
        compared to the standard font size.

        @see SetSymbolicSizeRelativeTo().

        @since 2.9.2
     */
    void SetSymbolicSize(wxFontSymbolicSize size);

    /**
        Sets the font size compared to the base font size.

        This is the same as SetSymbolicSize() except that it uses the given
        font size as the normal font size instead of the standard font size.

        @since 2.9.2
     */
    void SetSymbolicSizeRelativeTo(wxFontSymbolicSize size, int base);

    /**
        Sets underlining.

        @param underlined
            @true to underline, @false otherwise.

        @see GetUnderlined()
    */
    virtual void SetUnderlined(bool underlined);

    /**
        Sets strike-through attribute of the font.

        Currently stricken-through fonts are only supported in wxMSW, wxGTK, and wxOSX.

        @param strikethrough
            @true to add strike-through style, @false to remove it.

        @see GetStrikethrough()

        @since 2.9.4
    */
    virtual void SetStrikethrough(bool strikethrough);

    /**
        Sets the font weight.

        @param weight
            One of the ::wxFontWeight values.

        @see GetWeight()
    */
    virtual void SetWeight(wxFontWeight weight);

    /**
        Sets the font weight using an integer value.

        See ::wxFontWeight for a list of valid weight identifiers and their
        corresponding integer value.

        @param weight
            An integer value int the range 1-1000.

        @see GetNumericWeight()
    */
    virtual void SetNumericWeight(int weight);

    //@}


    /**
        Inequality operator.

        See @ref overview_refcount_equality "reference-counted object comparison" for
        more info.
    */
    bool operator!=(const wxFont& font) const;

    /**
        Equality operator.

        See @ref overview_refcount_equality "reference-counted object comparison" for
        more info.
    */
    bool operator==(const wxFont& font) const;

    /**
        Assignment operator, using @ref overview_refcount "reference counting".
    */
    wxFont& operator =(const wxFont& font);


    // statics

    /**
        Returns the current application's default encoding.

        @see @ref overview_fontencoding, SetDefaultEncoding()
    */
    static wxFontEncoding GetDefaultEncoding();

    /**
        Sets the default font encoding.

        @see @ref overview_fontencoding, GetDefaultEncoding()
    */
    static void SetDefaultEncoding(wxFontEncoding encoding);

    /**
        Get the raw weight value corresponding to the given symbolic constant.

        For compatibility, this function handles the values @c wxNORMAL, @c
        wxLIGHT and @c wxBOLD, that have values 90, 91 and 92, specially and
        converts them to the corresponding @c wxFONTWEIGHT_XXX weight value.

        @param weight
            A valid element of wxFontWeight enum, i.e. this argument can't have
            value ::wxFONTWEIGHT_INVALID.
        @return Numeric weight, between 1 and 1000.

        @since 3.1.2
     */
    static int GetNumericWeightOf(wxFontWeight weight);

    //@{
    /**
        This function takes the same parameters as the relative
        @ref wxFont::wxFont "wxFont constructor" and returns a new font
        object allocated on the heap.

        Their use is discouraged, use wxFont constructor from wxFontInfo
        instead.
    */
    static wxFont* New(int pointSize, wxFontFamily family, wxFontStyle style,
                       wxFontWeight weight,
                       bool underline = false,
                       const wxString& faceName = wxEmptyString,
                       wxFontEncoding encoding = wxFONTENCODING_DEFAULT);
    static wxFont* New(int pointSize, wxFontFamily family,
                       int flags = wxFONTFLAG_DEFAULT,
                       const wxString& faceName = wxEmptyString,
                       wxFontEncoding encoding = wxFONTENCODING_DEFAULT);
    static wxFont* New(const wxSize& pixelSize,
                       wxFontFamily family,
                       wxFontStyle style,
                       wxFontWeight weight,
                       bool underline = false,
                       const wxString& faceName = wxEmptyString,
                       wxFontEncoding encoding = wxFONTENCODING_DEFAULT);
    static wxFont* New(const wxSize& pixelSize,
                       wxFontFamily family,
                       int flags = wxFONTFLAG_DEFAULT,
                       const wxString& faceName = wxEmptyString,
                       wxFontEncoding encoding = wxFONTENCODING_DEFAULT);


    static wxFont *New(const wxNativeFontInfo& nativeInfo);
    static wxFont *New(const wxString& nativeInfoString);

    //@}
};


/**
    An empty wxFont.
*/
wxFont wxNullFont;

/**
    Equivalent to wxSystemSettings::GetFont(wxSYS_DEFAULT_GUI_FONT).

    @see wxSystemSettings
*/
wxFont* wxNORMAL_FONT;

/**
    A font using the @c wxFONTFAMILY_SWISS family and 2 points smaller than
    ::wxNORMAL_FONT.
*/
wxFont* wxSMALL_FONT;

/**
    A font using the @c wxFONTFAMILY_ROMAN family and @c wxFONTSTYLE_ITALIC style and
    of the same size of ::wxNORMAL_FONT.
*/
wxFont* wxITALIC_FONT;

/**
    A font identic to ::wxNORMAL_FONT except for the family used which is
    @c wxFONTFAMILY_SWISS.
*/
wxFont* wxSWISS_FONT;


/**
    @class wxFontList

    A font list is a list containing all fonts which have been created.
    There is only one instance of this class: ::wxTheFontList.

    Use this object to search for a previously created font of the desired type
    and create it if not already found.

    In some windowing systems, the font may be a scarce resource, so it is best to
    reuse old resources if possible.  When an application finishes, all fonts will
    be deleted and their resources freed, eliminating the possibility of 'memory
    leaks'.

    @library{wxcore}
    @category{gdi}

    @see wxFont
*/
class wxFontList
{
public:
    /**
        Constructor. The application should not construct its own font list:
        use the object pointer ::wxTheFontList.
    */
    wxFontList();

    /**
        Finds a font of the given specification, or creates one and adds it to the
        list. See the @ref wxFont "wxFont constructor" for details of the arguments.
    */
    wxFont* FindOrCreateFont(int point_size, wxFontFamily family, wxFontStyle style,
                             wxFontWeight weight, bool underline = false,
                             const wxString& facename = wxEmptyString,
                             wxFontEncoding encoding = wxFONTENCODING_DEFAULT);

    /**
        Finds a font of the given specification, or creates one and adds it to the
        list. See the @ref wxFont "wxFont constructor" for details of the arguments.

        @since 3.1.1
    */
    wxFont* FindOrCreateFont(const wxFontInfo& fontInfo);
};


/**
    The global wxFontList instance.
*/
wxFontList* wxTheFontList;


// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_misc */
//@{

/**
    Converts string to a wxFont best represented by the given string. Returns
    @true on success.

    @see wxToString(const wxFont&)

    @header{wx/font.h}
*/
bool wxFromString(const wxString& string, wxFont* font);

/**
    Converts the given wxFont into a string.

    @see wxFromString(const wxString&, wxFont*)

    @header{wx/font.h}
*/
wxString wxToString(const wxFont& font);

//@}

