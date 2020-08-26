/////////////////////////////////////////////////////////////////////////////
// Name:        fontmap.h
// Purpose:     interface of wxFontMapper
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxFontMapper

    wxFontMapper manages user-definable correspondence between logical font
    names and the fonts present on the machine.

    The default implementations of all functions will ask the user if they are
    not capable of finding the answer themselves and store the answer in a
    config file (configurable via SetConfigXXX functions). This behaviour may
    be disabled by giving the value of @false to "interactive" parameter.

    However, the functions will always consult the config file to allow the
    user-defined values override the default logic and there is no way to
    disable this - which shouldn't be ever needed because if "interactive" was
    never @true, the config file is never created anyhow.

    In case everything else fails (i.e. there is no record in config file
    and "interactive" is @false or user denied to choose any replacement),
    the class queries wxEncodingConverter for "equivalent" encodings
    (e.g. iso8859-2 and cp1250) and tries them.


    @section fontmapper_mbconv Using wxFontMapper in conjunction with wxMBConv classes

    If you need to display text in encoding which is not available at host
    system (see wxFontMapper::IsEncodingAvailable), you may use these two
    classes to find font in some similar encoding (see wxFontMapper::GetAltForEncoding)
    and convert the text to this encoding (wxMBConv classes).
    Following code snippet demonstrates it:

    @code
    if (!wxFontMapper::Get()->IsEncodingAvailable(enc, facename))
    {
        wxFontEncoding alternative;
        if (wxFontMapper::Get()->GetAltForEncoding(enc, &alternative,
                                                    facename, false))
        {
            wxCSConv convFrom(wxFontMapper::Get()->GetEncodingName(enc));
            wxCSConv convTo(wxFontMapper::Get()->GetEncodingName(alternative));
            text = wxString(text.mb_str(convFrom), convTo);
        }
        else
            ...failure (or we may try iso8859-1/7bit ASCII)...
    }
    ...display text...
    @endcode

    @library{wxcore}
    @category{cfg}

    @see wxEncodingConverter, @ref overview_nonenglish
*/
class wxFontMapper
{
public:
    /**
        Default ctor.

        @note
        The preferred way of creating a wxFontMapper instance is to call wxFontMapper::Get().
    */
    wxFontMapper();

    /**
        Virtual dtor.
    */
    virtual ~wxFontMapper();

    /**
        Returns the encoding for the given charset (in the form of RFC 2046) or
        @c wxFONTENCODING_SYSTEM if couldn't decode it.

        Be careful when using this function with @a interactive set to @true
        (default value) as the function then may show a dialog box to the user which
        may lead to unexpected reentrancies and may also take a significantly longer
        time than a simple function call. For these reasons, it is almost always a bad
        idea to call this function from the event handlers for repeatedly generated
        events such as @c EVT_PAINT.
    */
    virtual wxFontEncoding CharsetToEncoding(const wxString& charset,
                                             bool interactive = true);

    /**
        Get the current font mapper object. If there is no current object, creates one.

        @see Set()
    */
    static wxFontMapper* Get();

    /**
        Returns the array of all possible names for the given encoding.

        The array is @NULL-terminated. IF it isn't empty, the first name in it is
        the canonical encoding name, i.e. the same string as returned by
        GetEncodingName().
    */
    static const wxChar** GetAllEncodingNames(wxFontEncoding encoding);

    //@{
    /**
        Find an alternative for the given encoding (which is supposed to not be
        available on this system). If successful, return @true and fill info
        structure with the parameters required to create the font, otherwise
        return @false.

        The first form is for wxWidgets' internal use while the second one
        is better suitable for general use -- it returns wxFontEncoding which
        can consequently be passed to wxFont constructor.
    */
    bool GetAltForEncoding(wxFontEncoding encoding,
                           wxNativeEncodingInfo* info,
                           const wxString& facename = wxEmptyString,
                           bool interactive = true);
    bool GetAltForEncoding(wxFontEncoding encoding,
                           wxFontEncoding* alt_encoding,
                           const wxString& facename = wxEmptyString,
                           bool interactive = true);
    //@}

    /**
        Returns the @e n-th supported encoding.

        Together with GetSupportedEncodingsCount() this method may be used
        to get all supported encodings.
    */
    static wxFontEncoding GetEncoding(size_t n);

    /**
        Return user-readable string describing the given encoding.
    */
    static wxString GetEncodingDescription(wxFontEncoding encoding);

    /**
        Return the encoding corresponding to the given internal name.

        This function is the inverse of GetEncodingName() and is intentionally
        less general than CharsetToEncoding(), i.e. it doesn't try to make any
        guesses nor ever asks the user. It is meant just as a way of restoring
        objects previously serialized using GetEncodingName().
    */
    static wxFontEncoding GetEncodingFromName(const wxString& encoding);

    /**
        Return internal string identifier for the encoding (see also
        wxFontMapper::GetEncodingDescription).

        @see GetEncodingFromName()
    */
    static wxString GetEncodingName(wxFontEncoding encoding);

    /**
        Returns the number of the font encodings supported by this class.
        Together with GetEncoding() this method may be used to get
        all supported encodings.
    */
    static size_t GetSupportedEncodingsCount();

    /**
        Check whether given encoding is available in given face or not.
        If no facename is given, find @e any font in this encoding.
    */
    virtual bool IsEncodingAvailable(wxFontEncoding encoding,
                                     const wxString& facename = wxEmptyString);

    /**
        Set the current font mapper object and return previous one (may be @NULL).
        This method is only useful if you want to plug-in an alternative font mapper
        into wxWidgets.

        @see Get()
    */
    static wxFontMapper* Set(wxFontMapper* mapper);

    /**
        Set the root config path to use (should be an absolute path).
    */
    void SetConfigPath(const wxString& prefix);

    /**
        The parent window for modal dialogs.
    */
    void SetDialogParent(wxWindow* parent);

    /**
        The title for the dialogs (note that default is quite reasonable).
    */
    void SetDialogTitle(const wxString& title);
};

