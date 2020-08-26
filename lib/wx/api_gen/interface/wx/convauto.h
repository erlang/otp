/////////////////////////////////////////////////////////////////////////////
// Name:        convauto.h
// Purpose:     interface of wxConvAuto
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Constants representing various BOM types.

    BOM is an abbreviation for "Byte Order Mark", a special Unicode character
    which may be inserted into the beginning of a text stream to indicate its
    encoding.

    @since 2.9.3
 */
enum wxBOM
{
    /**
        Unknown BOM.

        This is returned if BOM presence couldn't be determined and normally
        happens because not enough bytes of input have been analysed.
     */
    wxBOM_Unknown = -1,

    /**
        No BOM.

        The stream doesn't contain BOM character at all.
     */
    wxBOM_None,

    /**
        UTF-32 big endian BOM.

        The stream is encoded in big endian variant of UTF-32.
     */
    wxBOM_UTF32BE,

    /**
        UTF-32 little endian BOM.

        The stream is encoded in little endian variant of UTF-32.
     */
    wxBOM_UTF32LE,

    /**
        UTF-16 big endian BOM.

        The stream is encoded in big endian variant of UTF-16.
     */
    wxBOM_UTF16BE,

    /**
        UTF-16 little endian BOM.

        The stream is encoded in little endian variant of UTF-16.
     */
    wxBOM_UTF16LE,

    /**
        UTF-8 BOM.

        The stream is encoded in UTF-8.

        Notice that contrary to a popular belief, it's perfectly possible and,
        n fact, common under Microsoft Windows systems, to have a BOM in an
        UTF-8 stream: while it's not used to indicate the endianness of UTF-8
        stream (as it's byte-oriented), the BOM can still be useful just as an
        unambiguous indicator of UTF-8 being used.
     */
    wxBOM_UTF8
};

/**
    @class wxConvAuto

    This class implements a Unicode to/from multibyte converter capable of
    automatically recognizing the encoding of the multibyte text on input. The
    logic used is very simple: the class uses the BOM (byte order mark) if it's
    present and tries to interpret the input as UTF-8 otherwise. If this fails,
    the input is interpreted as being in the default multibyte encoding which
    can be specified in the constructor of a wxConvAuto instance and, in turn,
    defaults to the value of GetFallbackEncoding() if not explicitly given.

    For the conversion from Unicode to multibyte, the same encoding as was
    previously used for multibyte to Unicode conversion is reused. If there had
    been no previous multibyte to Unicode conversion, UTF-8 is used by default.
    Notice that once the multibyte encoding is automatically detected, it
    doesn't change any more, i.e. it is entirely determined by the first use of
    wxConvAuto object in the multibyte-to-Unicode direction. However creating a
    copy of wxConvAuto object, either via the usual copy constructor or
    assignment operator, or using wxMBConv::Clone(), resets the automatically
    detected encoding so that the new copy will try to detect the encoding of
    the input on first use.

    This class is used by default in wxWidgets classes and functions reading
    text from files such as wxFile, wxFFile, wxTextFile, wxFileConfig and
    various stream classes so the encoding set with its SetFallbackEncoding()
    method will affect how these classes treat input files. In particular, use
    this method to change the fall-back multibyte encoding used to interpret
    the contents of the files whose contents isn't valid UTF-8 or to disallow
    it completely.

    @library{wxbase}
    @category{data}

    @see @ref overview_mbconv
*/
class wxConvAuto : public wxMBConv
{
public:
    /**
        Constructs a new wxConvAuto instance. The object will try to detect the
        input of the multibyte text given to its wxMBConv::ToWChar() method
        automatically but if the automatic detection of Unicode encodings
        fails, the fall-back encoding @a enc will be used to interpret it as
        multibyte text.

        The default value of @a enc, @c wxFONTENCODING_DEFAULT, means that the
        global default value (which can be set using SetFallbackEncoding())
        should be used. As with that method, passing @c wxFONTENCODING_MAX
        inhibits using this encoding completely so the input multibyte text
        will always be interpreted as UTF-8 in the absence of BOM and the
        conversion will fail if the input doesn't form valid UTF-8 sequence.

        Another special value is @c wxFONTENCODING_SYSTEM which means to use
        the encoding currently used on the user system, i.e. the encoding
        returned by wxLocale::GetSystemEncoding(). Any other encoding will be
        used as is, e.g. passing @c wxFONTENCODING_ISO8859_1 ensures that
        non-UTF-8 input will be treated as latin1.
    */
    wxConvAuto(wxFontEncoding enc = wxFONTENCODING_DEFAULT);


    /**
        Return the detected BOM type.

        The BOM type is detected after sufficiently many initial bytes have
        passed through this conversion object so it will always return
        wxBOM_Unknown immediately after the object creation but may return a
        different value later.

        @since 2.9.3
    */
    wxBOM GetBOM() const;

    /**
        Return a pointer to the characters that makes up this BOM.

        The returned character count is 2, 3 or 4, or undefined if the return
        value is NULL.

        @param bom
            A valid BOM type, i.e. not wxBOM_Unknown or wxBOM_None.
        @param count
            A non-@NULL pointer receiving the number of characters in this BOM.
        @return
            Pointer to characters composing the BOM or @NULL if BOM is unknown
            or invalid. Notice that the returned string is not NUL-terminated
            and may contain embedded NULs so @a count must be used to handle it
            correctly.

        @since 2.9.3
    */
    const char* GetBOMChars(wxBOM bom, size_t* count);

    /**
        Disable the use of the fall back encoding: if the input doesn't have a
        BOM and is not valid UTF-8, the conversion will fail.
    */
    static void DisableFallbackEncoding();

    /**
        Returns the encoding used by default by wxConvAuto if no other encoding
        is explicitly specified in constructor. By default, returns
        @c wxFONTENCODING_ISO8859_1 but can be changed using
        SetFallbackEncoding().
    */
    static wxFontEncoding GetFallbackEncoding();

    /**
        Changes the encoding used by default by wxConvAuto if no other encoding
        is explicitly specified in constructor. The default value, which can be
        retrieved using GetFallbackEncoding(), is @c wxFONTENCODING_ISO8859_1.

        Special values of @c wxFONTENCODING_SYSTEM or @c wxFONTENCODING_MAX can
        be used for the @a enc parameter to use the encoding of the current
        user locale as fall back or not use any encoding for fall back at all,
        respectively (just as with the similar constructor parameter). However,
        @c wxFONTENCODING_DEFAULT can't be used here.
    */
    static void SetFallbackEncoding(wxFontEncoding enc);

    /**
        Return the BOM type of this buffer.

        This is a helper function which is normally only used internally by
        wxConvAuto but provided for convenience of the code that wants to
        detect the encoding of a stream by checking it for BOM presence on its
        own.

        @since 2.9.3
    */
    static wxBOM DetectBOM(const char *src, size_t srcLen);
};
