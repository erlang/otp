/////////////////////////////////////////////////////////////////////////////
// Name:        iconbndl.h
// Purpose:     interface of wxIconBundle
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxIconBundle

    This class contains multiple copies of an icon in different sizes.
    It is typically used in wxDialog::SetIcons and wxTopLevelWindow::SetIcons.

    @library{wxcore}
    @category{gdi}

    @stdobjects
    ::wxNullIconBundle
*/
class wxIconBundle : public wxGDIObject
{
public:
    /**
        The elements of this enum determine what happens if GetIcon() doesn't
        find the icon of exactly the requested size.

        @since 2.9.4
     */
    enum
    {
        /// Return invalid icon if exact size is not found.
        FALLBACK_NONE = 0,

        /// Return the icon of the system icon size if exact size is not found.
        /// May be combined with other non-NONE enum elements to determine what
        /// happens if the system icon size is not found neither.
        FALLBACK_SYSTEM = 1,

        /// Return the icon of closest larger size or, if there is no icon of
        /// larger size in the bundle, the closest icon of smaller size.
        FALLBACK_NEAREST_LARGER = 2
    };


    /**
        Default ctor.
    */
    wxIconBundle();

    /**
        Initializes the bundle with the icon(s) found in the file.
    */
    wxIconBundle(const wxString& file, wxBitmapType type = wxBITMAP_TYPE_ANY);

    /**
        Initializes the bundle with the icon(s) found in the stream.

        Notice that the @a stream must be seekable, at least if it contains
        more than one icon. The stream pointer is positioned after the last
        icon read from the stream when this function returns.

        @since 2.9.0
    */
    wxIconBundle(wxInputStream& stream, wxBitmapType type = wxBITMAP_TYPE_ANY);

    /**
        Initializes the bundle with a single icon.
    */
    wxIconBundle(const wxIcon& icon);

    /**
        Initializes the bundle with all sizes of a group icon with @a
        resourceName stored as an MS Windows resource in @a module.

        When @a module is 0, the current instance is used.

        @see AddIcon(const wxString&, WXHINSTANCE)

        @onlyfor{wxmsw}
        @since 3.1.1
    */
    wxIconBundle(const wxString& resourceName, WXHINSTANCE module);

    /**
        Copy constructor.
    */
    wxIconBundle(const wxIconBundle& ic);

    /**
        Destructor.
    */
    virtual ~wxIconBundle();

    /**
        Adds all the icons contained in the file to the bundle; if the
        collection already contains icons with the same width and height, they
        are replaced by the new ones.
    */
    void AddIcon(const wxString& file, wxBitmapType type = wxBITMAP_TYPE_ANY);

    /**
        Adds all the icons contained in the stream to the bundle; if the
        collection already contains icons with the same width and height, they
        are replaced by the new ones.

        Notice that, as well as in the constructor loading the icon bundle from
        stream, the @a stream must be seekable, at least if more than one icon
        is to be loaded from it.

        @since 2.9.0
    */
    void AddIcon(wxInputStream& stream, wxBitmapType type = wxBITMAP_TYPE_ANY);

    /**
        Loads all sizes of a group icon with @a resourceName stored as an MS
        Windows resource in @a module.

        When @a module is 0, the current instance is used.

        @onlyfor{wxmsw}
        @since 3.1.1
    */
    void AddIcon(const wxString& resourceName, WXHINSTANCE module);

    /**
        Adds the icon to the collection; if the collection already
        contains an icon with the same width and height, it is
        replaced by the new one.
    */
    void AddIcon(const wxIcon& icon);

    /**
        Returns the icon with the given size.

        If @a size is ::wxDefaultSize, it is interpreted as the standard system
        icon size, i.e. the size returned by wxSystemSettings::GetMetric() for
        @c wxSYS_ICON_X and @c wxSYS_ICON_Y.

        If the bundle contains an icon with exactly the requested size, it's
        always returned. Otherwise, the behaviour depends on the flags. If only
        wxIconBundle::FALLBACK_NONE is given, the function returns an invalid
        icon. If wxIconBundle::FALLBACK_SYSTEM is given, it tries to find the
        icon of standard system size, regardless of the size passed as
        parameter. Otherwise, or if the icon system size is not found neither,
        but wxIconBundle::FALLBACK_NEAREST_LARGER flag is specified, the
        function returns the smallest icon of the size larger than the
        requested one or, if this fails too, just the icon closest to the
        specified size.

        The @a flags parameter is available only since wxWidgets 2.9.4.
    */
    wxIcon GetIcon(const wxSize& size, int flags = FALLBACK_SYSTEM) const;

    /**
        Same as @code GetIcon( wxSize( size, size ) ) @endcode.
    */
    wxIcon GetIcon(wxCoord size = wxDefaultCoord,
                   int flags = FALLBACK_SYSTEM) const;

    /**
        Returns the icon with exactly the given size or ::wxNullIcon if this
        size is not available.
    */
    wxIcon GetIconOfExactSize(const wxSize& size) const;

    /**
       return the number of available icons
    */
    size_t GetIconCount() const;

    /**
       return the icon at index (must be < GetIconCount())
    */
    wxIcon GetIconByIndex(size_t n) const;

    /**
        Returns @true if the bundle doesn't contain any icons, @false otherwise
        (in which case a call to GetIcon() with default parameter should return
        a valid icon).
    */
    bool IsEmpty() const;

    /**
        Assignment operator, using @ref overview_refcount "reference counting".
    */
    wxIconBundle& operator=(const wxIconBundle& ic);

};


/**
    An empty wxIconBundle.
*/
wxIconBundle wxNullIconBundle;


