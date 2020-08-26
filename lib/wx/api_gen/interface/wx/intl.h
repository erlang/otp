/////////////////////////////////////////////////////////////////////////////
// Name:        intl.h
// Purpose:     interface of wxLocale
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    This is the layout direction stored in wxLanguageInfo and returned by
    wxApp::GetLayoutDirection(), wxWindow::GetLayoutDirection(),
    wxDC::GetLayoutDirection() for RTL (right-to-left) languages support.
*/
enum wxLayoutDirection
{
    wxLayout_Default,
    wxLayout_LeftToRight,
    wxLayout_RightToLeft
};

/**
    Encapsulates a ::wxLanguage identifier together with OS-specific information
    related to that language.

    @beginWxPerlOnly
    In wxPerl @c Wx::LanguageInfo has only one method:
    - Wx::LanguageInfo->new(language, canonicalName, WinLang, WinSubLang, Description)
    @endWxPerlOnly
*/
struct wxLanguageInfo
{
    /// ::wxLanguage id.
    /// It should be greater than @c wxLANGUAGE_USER_DEFINED when defining your own
    /// language info structure.
    int Language;

    /// Canonical name of the language, e.g. @c fr_FR.
    wxString CanonicalName;

    //@{
    /**
        Win32 language identifiers (LANG_xxxx, SUBLANG_xxxx).

        @onlyfor{wxmsw}
    */
    wxUint32 WinLang, WinSublang;
    //@}

    /// Human-readable name of the language.
    wxString Description;

    /// The layout direction used for this language.
    wxLayoutDirection LayoutDirection;

    /// Return the LCID corresponding to this language.
    /// @onlyfor{wxmsw}
    wxUint32 GetLCID() const;

    /**
        Return the locale name corresponding to this language usable with
        @c setlocale() on the current system.

        If setting locale for this language is not supported, the returned
        string is empty.
     */
    wxString GetLocaleName() const;
};


/**
    The category of locale settings.

    @see wxLocale::GetInfo()
*/
enum wxLocaleCategory
{
    /// Number formatting.
    wxLOCALE_CAT_NUMBER,

    /// Date/time formatting.
    wxLOCALE_CAT_DATE,

    /// Monetary values formatting.
    wxLOCALE_CAT_MONEY,

    /**
        Default category for the wxLocaleInfo value.

        This category can be used for values which only make sense for a single
        category, e.g. wxLOCALE_SHORT_DATE_FMT which can only be used with
        wxLOCALE_CAT_DATE. As this is the default value of the second parameter
        of wxLocale::GetInfo(), wxLOCALE_CAT_DATE can be omitted when asking
        for wxLOCALE_SHORT_DATE_FMT value.

        @since 2.9.0
     */
    wxLOCALE_CAT_DEFAULT
};

/**
    The values understood by wxLocale::GetInfo().

    Note that for the @c wxLOCALE_*_FMT constants (the date and time formats),
    the strings returned by wxLocale::GetInfo() use strftime() or,
    equivalently, wxDateTime::Format() format. If the relevant format
    couldn't be determined, an empty string is returned -- there is no
    fallback value so that the application could determine the best course
    of actions itself in such case.

    All of these values are used with @c wxLOCALE_CAT_DATE in wxLocale::GetInfo() or,
    more typically, with @c wxLOCALE_CAT_DEFAULT as they only apply to a single category.
*/
enum wxLocaleInfo
{
    /**
        The thousands separator.

        This value can be used with either wxLOCALE_CAT_NUMBER or
        wxLOCALE_CAT_MONEY categories.
     */
    wxLOCALE_THOUSANDS_SEP,

    /**
        The character used as decimal point.

        This value can be used with either wxLOCALE_CAT_NUMBER or
        wxLOCALE_CAT_MONEY categories.
     */
    wxLOCALE_DECIMAL_POINT,

    /**
        Short date format.

        Notice that short and long date formats may be the same under POSIX
        systems currently but may, and typically are, different under MSW or macOS.

        @since 2.9.0
     */
    wxLOCALE_SHORT_DATE_FMT,

    /**
        Long date format.

        @since 2.9.0
     */
    wxLOCALE_LONG_DATE_FMT,

    /**
        Date and time format.

        @since 2.9.0
     */
    wxLOCALE_DATE_TIME_FMT,

    /**
        Time format.

        @since 2.9.0
     */
    wxLOCALE_TIME_FMT
};


/**
    @class wxLocale

    wxLocale class encapsulates all language-dependent settings and is a
    generalization of the C locale concept.

    In wxWidgets this class manages current locale. It also initializes and
    activates wxTranslations object that manages message catalogs.

    For a list of the supported languages, please see ::wxLanguage enum values.
    These constants may be used to specify the language in wxLocale::Init and
    are returned by wxLocale::GetSystemLanguage.

    @beginWxPerlOnly
    In wxPerl you can't use the '_' function name, so
    the @c Wx::Locale module can export the @c gettext and
    @c gettext_noop under any given name.

    @code
      # this imports gettext ( equivalent to Wx::GetTranslation
      # and gettext_noop ( a noop )
      # into your module
      use Wx::Locale qw(:default);

      # ....

      # use the functions
      print gettext( "Panic!" );

      button = Wx::Button-new( window, -1, gettext( "Label" ) );
    @endcode

    If you need to translate a lot of strings, then adding gettext( ) around
    each one is a long task ( that is why _( ) was introduced ), so just choose
    a shorter name for gettext:

    @code
      use Wx::Locale 'gettext' = 't',
                     'gettext_noop' = 'gettext_noop';

      # ...

      # use the functions
      print t( "Panic!!" );

      # ...
    @endcode
    @endWxPerlOnly

    @library{wxbase}
    @category{cfg}

    @see @ref overview_i18n, @ref page_samples_internat, wxXLocale, wxTranslations
*/
enum wxLocaleInitFlags
{
    wxLOCALE_DONT_LOAD_DEFAULT = 0x0000,     ///< Don't load wxstd.mo catalog.
    wxLOCALE_LOAD_DEFAULT      = 0x0001      ///< Load wxstd.mo (done by default).
};

class wxLocale
{
public:
    /**
        This is the default constructor and it does nothing to initialize the object:
        Init() must be used to do that.
    */
    wxLocale();

    /**
        See Init() for parameters description.
    */
    wxLocale(int language, int flags = wxLOCALE_LOAD_DEFAULT);

    /**
        See Init() for parameters description.

        The call of this function has several global side effects which you should
        understand: first of all, the application locale is changed - note that this
        will affect many of standard C library functions such as printf() or strftime().
        Second, this wxLocale object becomes the new current global locale for the
        application and so all subsequent calls to ::wxGetTranslation() will try to
        translate the messages using the message catalogs for this locale.
    */
    wxLocale(const wxString& name,
             const wxString& shortName = wxEmptyString,
             const wxString& locale = wxEmptyString,
             bool bLoadDefault = true);

    /**
        The destructor, like the constructor, also has global side effects: the
        previously set locale is restored and so the changes described in
        Init() documentation are rolled back.
    */
    virtual ~wxLocale();

    /**
        Calls wxTranslations::AddCatalog(const wxString&).
    */
    bool AddCatalog(const wxString& domain);

    /**
        Calls wxTranslations::AddCatalog(const wxString&, wxLanguage).
    */
    bool AddCatalog(const wxString& domain, wxLanguage msgIdLanguage);

    /**
        Calls wxTranslations::AddCatalog(const wxString&, wxLanguage, const wxString&).
    */
    bool AddCatalog(const wxString& domain, wxLanguage msgIdLanguage,
                    const wxString& msgIdCharset);

    /**
        Calls wxFileTranslationsLoader::AddCatalogLookupPathPrefix().
    */
    static void AddCatalogLookupPathPrefix(const wxString& prefix);

    /**
        Adds custom, user-defined language to the database of known languages.
        This database is used in conjunction with the first form of Init().
    */
    static void AddLanguage(const wxLanguageInfo& info);

    /**
        This function may be used to find the language description structure for the
        given locale, specified either as a two letter ISO language code (for example,
        "pt"), a language code followed by the country code ("pt_BR") or a full, human
        readable, language description ("Portuguese-Brazil").

        Returns the information for the given language or @NULL if this language
        is unknown. Note that even if the returned pointer is valid, the caller
        should @e not delete it.

        @see GetLanguageInfo()
    */
    static const wxLanguageInfo* FindLanguageInfo(const wxString& locale);

    /**
        Returns the canonical form of current locale name. Canonical form is the
        one that is used on UNIX systems: it is a two- or five-letter string in xx or
        xx_YY format, where xx is ISO 639 code of language and YY is ISO 3166 code of
        the country. Examples are "en", "en_GB", "en_US" or "fr_FR".
        This form is internally used when looking up message catalogs.
        Compare GetSysName().
    */
    wxString GetCanonicalName() const;

    /**
        Calls wxTranslations::GetHeaderValue().
    */
    wxString GetHeaderValue(const wxString& header,
                            const wxString& domain = wxEmptyString) const;

    /**
        Returns the ::wxLanguage constant of current language.

        Note that you can call this function only if you used the form of
        Init() that takes ::wxLanguage argument.
    */
    int GetLanguage() const;

    /**
        Returns a pointer to wxLanguageInfo structure containing information about
        the given language or @NULL if this language is unknown. Note that even if
        the returned pointer is valid, the caller should @e not delete it.

        See AddLanguage() for the wxLanguageInfo description.
        As with Init(), @c wxLANGUAGE_DEFAULT has the special meaning if passed
        as an argument to this function and in this case the result of
        GetSystemLanguage() is used.
    */
    static const wxLanguageInfo* GetLanguageInfo(int lang);

    /**
        Returns English name of the given language or empty string if this
        language is unknown.

        See GetLanguageInfo() for a remark about special meaning of @c wxLANGUAGE_DEFAULT.
    */
    static wxString GetLanguageName(int lang);

    /**
        Returns canonical name (see GetCanonicalName()) of the given language
        or empty string if this language is unknown.

        See GetLanguageInfo() for a remark about special meaning of @c wxLANGUAGE_DEFAULT.

        @since 2.9.1
    */
    static wxString GetLanguageCanonicalName(int lang);

    /**
        Returns the locale name as passed to the constructor or Init().

        This is a full, human-readable name, e.g. "English" or "French".
    */
    const wxString& GetLocale() const;

    /**
        Returns the current short name for the locale (as given to the constructor or
        the Init() function).
    */
    const wxString& GetName() const;

    /**
        Calls wxGetTranslation(const wxString&, const wxString&).
    */
    const wxString& GetString(const wxString& origString,
                              const wxString& domain = wxEmptyString) const;

    /**
        Calls wxGetTranslation(const wxString&, const wxString&, unsigned, const wxString&).
    */
    const wxString& GetString(const wxString& origString,
                              const wxString& origString2, unsigned n,
                              const wxString& domain = wxEmptyString) const;

    /**
        Returns current platform-specific locale name as passed to setlocale().
        Compare GetCanonicalName().
    */
    wxString GetSysName() const;

    /**
        Tries to detect the user's default font encoding.
        Returns wxFontEncoding() value or @c wxFONTENCODING_SYSTEM if it
        couldn't be determined.
    */
    static wxFontEncoding GetSystemEncoding();

    /**
        Tries to detect the name of the user's default font encoding.
        This string isn't particularly useful for the application as its form is
        platform-dependent and so you should probably use GetSystemEncoding() instead.

        Returns a user-readable string value or an empty string if it couldn't be
        determined.
    */
    static wxString GetSystemEncodingName();

    /**
        Tries to detect the user's default locale setting.

        Returns the ::wxLanguage value or @c wxLANGUAGE_UNKNOWN if the language-guessing
        algorithm failed.

        @note This function works with @em locales and returns the user's default
              locale. This may be, and usually is, the same as their preferred UI
              language, but it's not the same thing. Use wxTranslation to obtain
              @em language information.

        @see wxTranslations::GetBestTranslation().
    */
    static int GetSystemLanguage();

    /**
        Get the values of the given locale-dependent datum.

        This function returns the value of the locale-specific option specified
        by the given @a index.

        @param index
            One of the elements of wxLocaleInfo enum.
        @param cat
            The category to use with the given index or wxLOCALE_CAT_DEFAULT if
            the index can only apply to a single category.
        @return
            The option value or empty string if the function failed.
    */
    static wxString GetInfo(wxLocaleInfo index,
                            wxLocaleCategory cat = wxLOCALE_CAT_DEFAULT);

    /**
        Get the values of a locale datum in the OS locale.

        This function is similar to GetInfo() and, in fact, identical to it
        under non-MSW systems. Under MSW it differs from it when no locale had
        been explicitly set: GetInfo() returns the values corresponding to the
        "C" locale used by the standard library functions, while this method
        returns the values used by the OS which, in Windows case, correspond to
        the user settings in the control panel.

        @since 3.1.0
     */
    static wxString GetOSInfo(wxLocaleInfo index,
                              wxLocaleCategory cat = wxLOCALE_CAT_DEFAULT);

    /**
        Initializes the wxLocale instance.

        The call of this function has several global side effects which you should
        understand: first of all, the application locale is changed - note that
        this will affect many of standard C library functions such as printf()
        or strftime().
        Second, this wxLocale object becomes the new current global locale for
        the application and so all subsequent calls to wxGetTranslation() will
        try to translate the messages using the message catalogs for this locale.

        @param language
            ::wxLanguage identifier of the locale.
            @c wxLANGUAGE_DEFAULT has special meaning -- wxLocale will use system's
            default language (see GetSystemLanguage()).
        @param flags
            Combination of the following:
            - wxLOCALE_LOAD_DEFAULT: Load the message catalog for the given locale
              containing the translations of standard wxWidgets messages
              automatically.
            - wxLOCALE_DONT_LOAD_DEFAULT: Negation of wxLOCALE_LOAD_DEFAULT.

        @return @true on success or @false if the given locale couldn't be set.
    */
    bool Init(int language = wxLANGUAGE_DEFAULT,
              int flags = wxLOCALE_LOAD_DEFAULT);

    /**
        @deprecated
        This form is deprecated, use the other one unless you know what you are doing.

        @param name
            The name of the locale. Only used in diagnostic messages.
        @param shortName
            The standard 2 letter locale abbreviation; it is used as the
            directory prefix when looking for the message catalog files.
        @param locale
            The parameter for the call to setlocale().
            Note that it is platform-specific.
        @param bLoadDefault
            May be set to @false to prevent loading of the message catalog for the
            given locale containing the translations of standard wxWidgets messages.
            This parameter would be rarely used in normal circumstances.
    */
    bool Init(const wxString& name, const wxString& shortName = wxEmptyString,
              const wxString& locale = wxEmptyString, bool bLoadDefault = true);

    /**
        Check whether the operating system and/or C run time environment supports
        this locale. For example in Windows, support for many
        locales is not installed by default. Returns @true if the locale is
        supported.

        The argument @a lang is the ::wxLanguage identifier. To obtain this for a
        given a two letter ISO language code, use FindLanguageInfo() to obtain its
        wxLanguageInfo structure.
        See AddLanguage() for the wxLanguageInfo description.

        @since 2.7.1.
    */
    static bool IsAvailable(int lang);

    /**
        Calls wxTranslations::IsLoaded().
    */
    bool IsLoaded(const wxString& domain) const;

    /**
        Returns @true if the locale could be set successfully.
    */
    bool IsOk() const;
};



/**
   Get the current locale object (note that it may be NULL!)
*/
wxLocale* wxGetLocale();

