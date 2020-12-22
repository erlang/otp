/////////////////////////////////////////////////////////////////////////////
// Name:        strconv.h
// Purpose:     interface of wxMBConvUTF7
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxMBConv

    This class is the base class of a hierarchy of classes capable of
    converting text strings between multibyte (SBCS or DBCS) encodings and
    Unicode.

    This is an abstract base class which defines the operations implemented by
    all different conversion classes. The derived classes don't add any new
    operations of their own (except, possibly, some non-default constructors)
    and so you should simply use this class ToWChar() and FromWChar() (or
    cMB2WC() and cWC2MB()) methods with the objects of the derived class.

    In the documentation for this and related classes please notice that
    length of the string refers to the number of characters in the string
    not counting the terminating @c NUL, if any. While the size of the string
    is the total number of bytes in the string, including any trailing @c NUL.
    Thus, length of wide character string @c L"foo" is 3 while its size can
    be either 8 or 16 depending on whether @c wchar_t is 2 bytes (as
    under Windows) or 4 (Unix).

    @library{wxbase}
    @category{conv}

    @see wxCSConv, wxEncodingConverter, @ref overview_mbconv
*/
class wxMBConv
{
public:
    /**
        Trivial default constructor.
    */
    wxMBConv();

    /**
        This pure virtual function is overridden in each of the derived classes
        to return a new copy of the object it is called on.

        It is used for copying the conversion objects while preserving their
        dynamic type.
    */
    virtual wxMBConv* Clone() const = 0;

    /**
        This function must be overridden in the derived classes to return the
        maximum length, in bytes, of a single Unicode character representation
        in this encoding.

        As a consequence, the conversion object must be able to decode any
        valid sequence of bytes in the corresponding encoding if it's at least
        that many bytes long, but may fail if it is shorter. For example, for
        UTF-8 the maximum character length is 4, as 3 bytes or less may be
        insufficient to represent a Unicode character in UTF-8, but 4 are
        always enough.

        For compatibility reasons, this method is not pure virtual and returns
        1 by default in the base class, however it should be always overridden
        in the derived classes.

        @since 3.1.3
     */
    virtual size_t GetMaxCharLen() const;

    /**
        This function returns 1 for most of the multibyte encodings in which the
        string is terminated by a single @c NUL, 2 for UTF-16 and 4 for UTF-32 for
        which the string is terminated with 2 and 4 @c NUL characters respectively.
        The other cases are not currently supported and @c wxCONV_FAILED
        (defined as -1) is returned for them.
    */
    virtual size_t GetMBNulLen() const;

    /**
        Returns the maximal value which can be returned by GetMBNulLen() for
        any conversion object.

        Currently this value is 4.

        This method can be used to allocate the buffer with enough space for the
        trailing @c NUL characters for any encoding.
    */
    static size_t GetMaxMBNulLen();

    /**
        Return true if the converter's charset is UTF-8.

        This is provided to optimize creating wxStrings from the @c char*
        strings returned by this converter, as they can be directly used with
        wxString::FromUTF8() or even wxString::FromUTF8Unchecked() when this
        method returns @true.

        This function is universally available since wxWidgets 3.1.1 (it was
        previously only available in some of the build configurations).
    */
    virtual bool IsUTF8() const;

    /**
        Convert multibyte string to a wide character one.

        This is the most general function for converting a multibyte string to
        a wide string, cMB2WC() may be often more convenient, however this
        function is the most efficient one as it allows avoiding any
        unnecessary copying.

        The main case is when @a dst is not @NULL and @a srcLen is not
        @c wxNO_LEN (which is defined as @c (size_t)-1): then the function
        converts exactly @a srcLen bytes starting at @a src into wide string
        which it output to @e dst. If the length of the resulting wide
        string is greater than @e dstLen, an error is returned. Note that if
        @a srcLen bytes don't include @c NUL characters, the resulting wide
        string is not @c NUL-terminated neither.

        If @a srcLen is @c wxNO_LEN, the function supposes that the string is
        properly (i.e. as necessary for the encoding handled by this
        conversion) @c NUL-terminated and converts the entire string, including
        any trailing @c NUL bytes. In this case the wide string is also @c
        NUL-terminated.

        Finally, if @a dst is @NULL, the function returns the length of the
        needed buffer.

        Example of use of this function:
        @code
        size_t dstLen = conv.ToWChar(NULL, 0, src);
        if ( dstLen == wxCONV_FAILED )
            ... handle error ...
        wchar_t *dst = new wchar_t[dstLen];
        if ( conv.ToWChar(dst, dstLen, src) == wxCONV_FAILED )
            ... handle error ...
        @endcode

        Notice that when passing the explicit source length the output will
        @e not be @c NUL terminated if you pass @c strlen(str) as parameter.
        Either leave @a srcLen as default @c wxNO_LEN or add one to @c strlen
        result if you want the output to be @c NUL terminated.

        @param dst
            Pointer to output buffer of the size of at least @a dstLen or @NULL.
        @param dstLen
            Maximal number of characters to be written to the output buffer if
            @a dst is non-@NULL, unused otherwise.
        @param src
            Point to the source string, must not be @NULL.
        @param srcLen
            The number of characters of the source string to convert or
            @c wxNO_LEN (default parameter) to convert everything up to and
            including the terminating @c NUL character(s).

        @return
            The number of characters written (or which would have been written
            if it were non-@NULL) to @a dst or @c wxCONV_FAILED on error.
    */
    virtual size_t ToWChar(wchar_t* dst, size_t dstLen, const char* src,
                           size_t srcLen = wxNO_LEN) const;

    /**
        Converts wide character string to multibyte.

        This function has the same semantics as ToWChar() except that it
        converts a wide string to multibyte one. As with ToWChar(), it may be
        more convenient to use cWC2MB() when working with @c NUL terminated
        strings.

        @param dst
            Pointer to output buffer of the size of at least @a dstLen or @NULL.
        @param dstLen
            Maximal number of characters to be written to the output buffer if
            @a dst is non-@NULL, unused otherwise.
        @param src
            Point to the source string, must not be @NULL.
        @param srcLen
            The number of characters of the source string to convert or
            @c wxNO_LEN (default parameter) to convert everything up to and
            including the terminating @c NUL character.

        @return
            If @a dst is non-@NULL, the number of characters actually written to
            it. If @a dst is @NULL, the returned value is at least equal to the
            number of characters that would have been written out if it were
            non-@NULL, but can be larger than it under the platforms using
            UTF-16 as @c wchar_t encoding (this allows a useful optimization in
            the implementation of this function for UTF-32). In any case,
            @c wxCONV_FAILED is returned on conversion error.
    */
    virtual size_t FromWChar(char* dst, size_t dstLen, const wchar_t* src,
                             size_t srcLen = wxNO_LEN) const;

    /**
        Converts from multibyte encoding to Unicode by calling ToWChar() and
        allocating a temporary wxWCharBuffer to hold the result.

        This function is a convenient wrapper around ToWChar() as it takes care
        of allocating the buffer of the necessary size itself. Its parameters
        have the same meaning as for ToWChar(), in particular @a inLen can be
        specified explicitly in which case exactly that many characters are
        converted and @a outLen receives (if non-@NULL) exactly the
        corresponding number of wide characters, whether the last one of them
        is @c NUL or not. However if @c inLen is @c wxNO_LEN, then @c outLen
        doesn't count the trailing @c NUL even if it is always present in this
        case.

        Finally notice that if the conversion fails, the returned buffer is
        invalid and @a outLen is set to 0 (and not @c wxCONV_FAILED for
        compatibility concerns).
    */
    wxWCharBuffer cMB2WC(const char* in,
                               size_t inLen,
                               size_t *outLen) const;

    /**
        Converts a char buffer to wide char one.

        This is the most convenient and safest conversion function as you
        don't have to deal with the buffer lengths directly. Use it if the
        input buffer is known not to be empty or if you are sure that the
        conversion is going to succeed -- otherwise, use the overload above to
        be able to distinguish between empty input and conversion failure.

        @return
            The buffer containing the converted text, empty if the input was
            empty or if the conversion failed.

        @since 2.9.1
     */
    wxWCharBuffer cMB2WC(const wxCharBuffer& buf) const;

    //@{
    /**
        Converts from multibyte encoding to the current wxChar type (which
        depends on whether wxUSE_UNICODE is set to 1).

        If wxChar is char, it returns the parameter unaltered. If wxChar is
        wchar_t, it returns the result in a wxWCharBuffer. The macro wxMB2WXbuf
        is defined as the correct return type (without const).
    */
    const char* cMB2WX(const char* psz) const;
    wxWCharBuffer cMB2WX(const char* psz) const;
    //@}

    /**
        Converts from Unicode to multibyte encoding by calling FromWChar() and
        allocating a temporary wxCharBuffer to hold the result.

        This function is a convenient wrapper around FromWChar() as it takes
        care of allocating the buffer of necessary size itself.

        Its parameters have the same meaning as the corresponding parameters of
        FromWChar(), please see the description of cMB2WC() for more details.
    */
    wxCharBuffer cWC2MB(const wchar_t* in,
                              size_t inLen,
                              size_t *outLen) const;

    /**
        Converts a wide char buffer to char one.

        This is the most convenient and safest conversion function as you
        don't have to deal with the buffer lengths directly. Use it if the
        input buffer is known not to be empty or if you are sure that the
        conversion is going to succeed -- otherwise, use the overload above to
        be able to distinguish between empty input and conversion failure.

        @return
            The buffer containing the converted text, empty if the input was
            empty or if the conversion failed.

        @since 2.9.1
     */
    wxCharBuffer cWC2MB(const wxWCharBuffer& buf) const;

    //@{
    /**
        Converts from Unicode to the current wxChar type.

        If wxChar is wchar_t, it returns the parameter unaltered. If wxChar is
        char, it returns the result in a wxCharBuffer. The macro wxWC2WXbuf is
        defined as the correct return type (without const).
    */
    const wchar_t* cWC2WX(const wchar_t* psz) const;
    wxCharBuffer cWC2WX(const wchar_t* psz) const;
    //@}

    //@{
    /**
        Converts from the current wxChar type to multibyte encoding.

        If wxChar is char, it returns the parameter unaltered. If wxChar is
        wchar_t, it returns the result in a wxCharBuffer. The macro wxWX2MBbuf
        is defined as the correct return type (without const).
    */
    const char* cWX2MB(const wxChar* psz) const;
    wxCharBuffer cWX2MB(const wxChar* psz) const;
    //@}

    //@{
    /**
        Converts from the current wxChar type to Unicode.

        If wxChar is wchar_t, it returns the parameter unaltered. If wxChar is
        char, it returns the result in a wxWCharBuffer. The macro wxWX2WCbuf is
        defined as the correct return type (without const).
    */
    const wchar_t* cWX2WC(const wxChar* psz) const;
    wxWCharBuffer cWX2WC(const wxChar* psz) const;
    //@}

    /**
        @deprecated This function is deprecated, please use ToWChar() instead.

        Converts from a string @a in multibyte encoding to Unicode putting up to
        @a outLen characters into the buffer @e out.

        If @a out is @NULL, only the length of the string which would result
        from the conversion is calculated and returned. Note that this is the
        length and not size, i.e. the returned value does not include the
        trailing @c NUL. But when the function is called with a non-@NULL @a
        out buffer, the @a outLen parameter should be one more to allow to
        properly @c NUL-terminate the string.

        So to properly use this function you need to write:
        @code
            size_t lenConv = conv.MB2WC(NULL, in, 0);
            if ( lenConv == wxCONV_FAILED )
                ... handle error ...
            // allocate 1 more character for the trailing NUL and also pass
            // the size of the buffer to the function now
            wchar_t *out = new wchar_t[lenConv + 1];
            if ( conv.MB2WC(out, in, lenConv + 1) == wxCONV_FAILED )
                ... handle error ...
        @endcode
        For this and other reasons, ToWChar() is strongly recommended as a
        replacement.

        @param out
            The output buffer, may be @NULL if the caller is only
            interested in the length of the resulting string
        @param in
            The NUL-terminated input string, cannot be @NULL
        @param outLen
            The length of the output buffer but including
            NUL, ignored if out is @NULL

        @return The length of the converted string excluding the trailing NUL.
    */
    virtual size_t MB2WC(wchar_t* out, const char* in, size_t outLen) const;

    /**
        @deprecated This function is deprecated, please use FromWChar() instead.

        Converts from Unicode to multibyte encoding.
        The semantics of this function (including the return value meaning) is
        the same as for wxMBConv::MB2WC. Notice that when the function is
        called with a non-@NULL buffer, the @a n parameter should be the size
        of the buffer and so it should take into account the trailing @c NUL,
        which might take two or four bytes for some encodings (UTF-16 and
        UTF-32) and not one, i.e. GetMBNulLen().
    */
    virtual size_t WC2MB(char* buf, const wchar_t* psz, size_t n) const;
};


/**
    @class wxMBConvUTF7

    This class converts between the UTF-7 encoding and Unicode.
    It has one predefined instance, @b wxConvUTF7.

    Notice that, unlike all the other conversion objects, this converter is
    stateful, i.e. it remembers its state from the last call to its ToWChar()
    or FromWChar() and assumes it is called on the continuation of the same
    string when the same method is called again. This assumption is only made
    if an explicit length is specified as parameter to these functions as if an
    entire @c NUL terminated string is processed the state doesn't need to be
    remembered.

    This also means that, unlike the other predefined conversion objects,
    @b wxConvUTF7 is @em not thread-safe.

    @library{wxbase}
    @category{conv}

    @see wxMBConvUTF8, @ref overview_mbconv
*/
class wxMBConvUTF7 : public wxMBConv
{
};



/**
    @class wxMBConvUTF8

    This class converts between the UTF-8 encoding and Unicode.
    It has one predefined instance, @b wxConvUTF8.

    @library{wxbase}
    @category{conv}

    @see wxMBConvUTF7, @ref overview_mbconv
*/
class wxMBConvUTF8 : public wxMBConv
{
};



/**
    @class wxMBConvUTF16

    This class is used to convert between multibyte encodings and UTF-16 Unicode
    encoding (also known as UCS-2).

    Unlike UTF-8 encoding, UTF-16 uses words and not bytes and hence depends
    on the byte ordering: big or little endian. Hence this class is provided in
    two versions: wxMBConvUTF16LE and wxMBConvUTF16BE and wxMBConvUTF16 itself
    is just a typedef for one of them (native for the given platform, e.g. LE
    under Windows and BE under Mac).

    @library{wxbase}
    @category{conv}

    @see wxMBConvUTF8, wxMBConvUTF32, @ref overview_mbconv
*/
class wxMBConvUTF16 : public wxMBConv
{
};


/**
    @class wxMBConvUTF32

    This class is used to convert between multibyte encodings and UTF-32
    Unicode encoding (also known as UCS-4).
    Unlike UTF-8 encoding, UTF-32 uses (double) words and not bytes and hence
    depends on the byte ordering: big or little endian. Hence this class is
    provided in two versions: wxMBConvUTF32LE and wxMBConvUTF32BE and
    wxMBConvUTF32 itself is just a typedef for one of them (native for the
    given platform, e.g. LE under Windows and BE under Mac).

    @library{wxbase}
    @category{conv}

    @see wxMBConvUTF8, wxMBConvUTF16, @ref overview_mbconv
*/
class wxMBConvUTF32 : public wxMBConv
{
};




/**
    @class wxCSConv

    This class converts between any character set supported by the system and
    Unicode.

    Please notice that this class uses system-provided conversion functions,
    e.g. @c MultiByteToWideChar() and @c WideCharToMultiByte() under MSW and @c
    iconv(3) under Unix systems and as such may support different encodings and
    different encoding names on different platforms (although all relatively
    common encodings are supported should be supported everywhere).

    It has one predefined instance, @b wxConvLocal, for the default user
    character set.

    @library{wxbase}
    @category{conv}

    @see wxMBConv, wxEncodingConverter, @ref overview_mbconv
*/
class wxCSConv : public wxMBConv
{
public:
    /**
        Constructor.

        You can specify the name of the character set you want to convert
        from/to. If the character set name is not recognized, ISO 8859-1 is
        used as fall back, use IsOk() to test for this.

        @param charset The name of the encoding, shouldn't be empty.
    */
    wxCSConv(const wxString& charset);

    /**
        Constructor.

        You can specify an encoding constant for the character set you want to
        convert from/to. Use IsOk() after construction to check whether the
        encoding is supported by the current system.

        @param encoding Any valid (i.e. not wxFONTENCODING_MAX) font encoding.
    */
    wxCSConv(wxFontEncoding encoding);

    /**
        Returns @true if the charset (or the encoding) given at constructor is
        really available to use.

        Returns @false if ISO 8859-1 will be used instead.

        Note this does not mean that a given string will be correctly
        converted. A malformed string may still make conversion functions
        return @c wxCONV_FAILED.

        @since 2.8.2
    */
    bool IsOk() const;
};

/**
    Conversion object always producing non-empty output for non-empty input.

    Conversions done using this object never lose data, at the cost of possibly
    producing the output in an unwanted encoding or misinterpreting input
    encoding.

    To be precise, converting Unicode to multibyte strings using this object
    tries to use the current locale encoding first but if this doesn't work, it
    falls back to using UTF-8. In the other direction, UTF-8 is tried first,
    then the current locale encoding and if this fails too, input is
    interpreted as using ISO 8859-1, which never fails.

    It is almost always @e wrong to use this converter for multibyte-to-Unicode
    direction as the program should know which encoding the input data is
    supposed to use and use the appropriate converter instead. However it may
    be useful in the Unicode-to-multibyte direction if the goal is to produce
    the output in the current locale encoding if possible, but still output
    something, instead of nothing at all, even if the Unicode string is not
    representable in this encoding.

    @since 3.1.0
 */
extern wxMBConv& wxConvWhateverWorks;


/**
    Conversion object used for converting file names from their external
    representation to the one used inside the program.

    @b wxConvFileName converts filenames between filesystem multibyte encoding
    and Unicode. @b wxConvFileName can also be set to a something else at
    run-time which is used e.g. by wxGTK to use an object which checks the
    environment variable @b G_FILESYSTEM_ENCODING indicating that filenames
    should not be interpreted as UTF8 and also for converting invalid UTF8
    characters (e.g. if there is a filename in iso8859_1) to strings with octal
    values.

    Since some platforms (such as Win32) use Unicode in the filenames,
    and others (such as Unix) use multibyte encodings, this object should only
    be used directly if wxMBFILES is defined to 1. A convenience macro,
    @c wxFNCONV, is defined to @c wxConvFileName->cWX2MB in this case. You
    could use it like this:

    @code
    wxChar *name = "rawfile.doc";
    FILE *fil = fopen(wxFNCONV(name), "r");
    @endcode

    (although it would be better to just use wxFopen(name, "r") in this
    particular case, you only need to use this object for functions taking file
    names not wrapped by wxWidgets.)

    @library{wxbase}
    @category{conv}

    @see @ref overview_mbconv
*/
extern wxMBConv* wxConvFileName;
