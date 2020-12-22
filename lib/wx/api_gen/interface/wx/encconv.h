/////////////////////////////////////////////////////////////////////////////
// Name:        encconv.h
// Purpose:     interface of wxEncodingConverter
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxEncodingConverter

    This class is capable of converting strings between two 8-bit encodings/charsets.
    It can also convert from/to Unicode.

    Only a limited subset of encodings is supported by wxEncodingConverter:
    @c wxFONTENCODING_ISO8859_1..15, @c wxFONTENCODING_CP1250..1257 and
    @c wxFONTENCODING_KOI8.

    @note
    Please use wxMBConv classes instead if possible. wxCSConv has much better
    support for various encodings than wxEncodingConverter.
    wxEncodingConverter is useful only if you rely on wxCONVERT_SUBSTITUTE mode
    of operation (see wxEncodingConverter::Init()).

    @library{wxbase}
    @category{conv}

    @see wxFontMapper, wxMBConv, @ref overview_nonenglish
*/
class wxEncodingConverter : public wxObject
{
public:
    /**
        Constructor.
    */
    wxEncodingConverter();

    /**
        Return @true if (any text in) multibyte encoding @a encIn can be converted to
        another one (@a encOut) losslessly.

        Do not call this method with @c wxFONTENCODING_UNICODE as either parameter,
        it doesn't make sense (always works in one sense and always depends
        on the text to convert in the other).
    */
    static bool CanConvert(wxFontEncoding encIn,
                           wxFontEncoding encOut);

    /**
        @name Conversion functions

        @{
    */
    /**
        Convert input string according to settings passed to Init() and writes
        the result to output.

        All the Convert() function overloads return @true if the conversion was
        lossless and @false if at least one of the characters couldn't be converted
        was and replaced with '?' in the output.

        Note that if @c wxCONVERT_SUBSTITUTE was passed to Init(), substitution is
        considered a lossless operation.

        @note You must call Init() before using this method!
    */
    bool Convert(const char* input, char* output) const;
    bool Convert(const wchar_t* input, wchar_t* output) const;
    bool Convert(const char* input, wchar_t* output) const;
    bool Convert(const wchar_t* input, char* output) const;

    /**
        Convert input string according to settings passed to Init() in-place.

        With this overload, the conversion result is written to the same memory
        area from which the input is read.

        See the Convert(const char*,char*) const overload for more info.
    */
    bool Convert(char* str) const;

    /**
        Convert input string according to settings passed to Init() in-place.

        With this overload, the conversion result is written to the same memory
        area from which the input is read.

        See the Convert(const wchar_t*,wchar_t*) const overload for more info.
    */
    bool Convert(wchar_t* str) const;

    /**
        Convert a wxString and return a new wxString object.

        See the Convert(const char*,char*) const overload for more info.
    */
    wxString Convert(const wxString& input) const;
    //@}


    /**
        Similar to GetPlatformEquivalents(), but this one will return ALL
        equivalent encodings, regardless of the platform, and including itself.

        This platform's encodings are before others in the array.
        And again, if @a enc is in the array, it is the very first item in it.
    */
    static wxFontEncodingArray GetAllEquivalents(wxFontEncoding enc);

    /**
        Return equivalents for given font that are used under given platform.

        Supported platforms:
        @li wxPLATFORM_UNIX
        @li wxPLATFORM_WINDOWS
        @li wxPLATFORM_MAC
        @li wxPLATFORM_CURRENT

        wxPLATFORM_CURRENT means the platform this binary was compiled for.

        Examples:

        @verbatim
        current platform   enc          returned value
        ----------------------------------------------
        unix            CP1250             {ISO8859_2}
        unix         ISO8859_2             {ISO8859_2}
        windows      ISO8859_2                {CP1250}
        unix            CP1252  {ISO8859_1,ISO8859_15}
        @endverbatim

        Equivalence is defined in terms of convertibility: two encodings are
        equivalent if you can convert text between then without losing
        information (it may - and will - happen that you lose special chars
        like quotation marks or em-dashes but you shouldn't lose any diacritics
        and language-specific characters when converting between equivalent encodings).

        Remember that this function does @b NOT check for presence of
        fonts in system. It only tells you what are most suitable
        encodings. (It usually returns only one encoding.)

        @note Note that argument enc itself may be present in the returned array,
              so that you can, as a side-effect, detect whether the encoding is
              native for this platform or not.

        @note Convert() is not limited to converting between equivalent encodings,
              it can convert between two arbitrary encodings.

        @note If @a enc is present in the returned array, then it is always the first
              item of it.

        @note Please note that the returned array may contain no items at all.
    */
    static wxFontEncodingArray GetPlatformEquivalents(wxFontEncoding enc,
                                                      int platform = wxPLATFORM_CURRENT);

    /**
        Initialize the conversion.

        Both output or input encoding may be wxFONTENCODING_UNICODE, but only
        if wxUSE_ENCODING is set to 1.

        All subsequent calls to Convert() will interpret its argument
        as a string in @a input_enc encoding and will output string in
        @a output_enc encoding.

        You must call this method before calling Convert. You may call
        it more than once in order to switch to another conversion.

        @a method affects behaviour of Convert() in case input character
        cannot be converted because it does not exist in output encoding:

        @li @b wxCONVERT_STRICT: follow behaviour of GNU Recode - just copy
            unconvertible  characters to output and don't change them
            (its integer value will stay the same)
        @li @b wxCONVERT_SUBSTITUTE:  try some (lossy) substitutions - e.g.
            replace unconvertible latin capitals with acute by ordinary
            capitals, replace en-dash or em-dash by '-' etc.

        Both modes guarantee that output string will have same length
        as input string.

        @return @false if given conversion is impossible, @true otherwise
                (conversion may be impossible either if you try to convert
                to Unicode with non-Unicode build of wxWidgets or if input
                or output encoding is not supported).
    */
    bool Init(wxFontEncoding input_enc, wxFontEncoding output_enc,
              int method = wxCONVERT_STRICT);
};

