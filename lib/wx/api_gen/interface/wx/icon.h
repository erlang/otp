/////////////////////////////////////////////////////////////////////////////
// Name:        icon.h
// Purpose:     interface of wxIcon
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////



/**
    In wxIcon context this value means: "use the screen depth".
*/
#define wxICON_SCREEN_DEPTH       (-1)


/**
    @class wxIcon

    An icon is a small rectangular bitmap usually used for denoting a minimized
    application.

    It differs from a wxBitmap in always having a mask associated with it for
    transparent drawing. On some platforms, icons and bitmaps are implemented
    identically, since there is no real distinction between a wxBitmap with a
    mask and an icon; and there is no specific icon format on some platforms
    (X-based applications usually standardize on XPMs for small bitmaps and icons).
    However, some platforms (such as Windows) make the distinction, so a
    separate class is provided.

    @remarks
    It is usually desirable to associate a pertinent icon with a frame.
    Icons can also be used for other purposes, for example with wxTreeCtrl and wxListCtrl.
    Icons have different formats on different platforms therefore separate icons
    will usually be created for the different environments.
    Platform-specific methods for creating a wxIcon structure are catered for,
    and this is an occasion where conditional compilation will probably be required.
    Note that a new icon must be created for every time the icon is to be used
    for a new window. In Windows, the icon will not be reloaded if it has already
    been used.
    An icon allocated to a frame will be deleted when the frame is deleted.
    For more information please see @ref overview_bitmap.

    @library{wxcore}
    @category{gdi}

    @stdobjects
    ::wxNullIcon

    @see @ref overview_bitmap, @ref overview_bitmap_supportedformats,
         wxIconBundle, wxDC::DrawIcon, wxCursor
*/
class wxIcon : public wxGDIObject
{
public:
    /**
        Default ctor.

        Constructs an icon object with no data; an assignment or another member
        function such as LoadFile() must be called subsequently.
    */
    wxIcon();

    /**
        Copy ctor.
    */
    wxIcon(const wxIcon& icon);

    /*
        Creates an icon from the given data, which can be of arbitrary type.

    wxIcon(void* data, int type, int width, int height, int depth = -1);

        NOTE: this ctor is not implemented by all ports, is somewhat useless
              without further description of the "data" supported formats and
              uses 'int type' instead of wxBitmapType, so don't document it.
    */

    /**
        Creates an icon from an array of bits.
        You should only use this function for monochrome bitmaps (depth 1) in
        portable programs: in this case the bits parameter should contain an XBM image.

        For other bit depths, the behaviour is platform dependent: under Windows,
        the data is passed without any changes to the underlying CreateBitmap() API.
        Under other platforms, only monochrome bitmaps may be created using this
        constructor and wxImage should be used for creating colour bitmaps from
        static data.

        @param bits
            Specifies an array of pixel values.
        @param width
            The width of the image.
        @param height
            The height of the image.

        @beginWxPerlOnly
        In wxPerl use Wx::Icon->newBits(bits, width, height, depth = -1);
        @endWxPerlOnly

        @onlyfor{wxmsw,wxosx}
    */
    wxIcon(const char bits[], int width, int height);

    /**
        Creates a bitmap from XPM data.
        To use this constructor, you must first include an XPM file.
        For example, assuming that the file mybitmap.xpm contains an XPM array
        of character pointers called @e mybitmap:

        @code
        #include "mybitmap.xpm"
        ...
        wxIcon *icon = new wxIcon(mybitmap);
        @endcode

        A macro, wxICON, is available which creates an icon using an XPM on
        the appropriate platform, or an icon resource on Windows.

        @code
        wxIcon icon(wxICON(sample));

        // Equivalent to:
        #if defined(__WXGTK__) || defined(__WXMOTIF__)
        wxIcon icon(sample_xpm);
        #endif

        #if defined(__WXMSW__)
        wxIcon icon("sample");
        #endif
        @endcode

        @beginWxPerlOnly
        In wxPerl use Wx::Icon->newFromXPM(data).
        @endWxPerlOnly
    */
    wxIcon(const char* const* bits);

    /**
        Loads an icon from a file or resource.

        @param name
            This can refer to a resource name or a filename under MS Windows and X.
            Its meaning is determined by the @a type parameter.
        @param type
            May be one of the ::wxBitmapType values and indicates which type of
            bitmap should be loaded. See the note in the class detailed description.
            Note that the wxICON_DEFAULT_TYPE constant has different value under
            different wxWidgets ports. See the icon.h header for the value it takes
            for a specific port.
        @param desiredWidth
            Specifies the desired width of the icon. This parameter only has
            an effect in Windows where icon resources can contain several icons
            of different sizes.
        @param desiredHeight
            Specifies the desired height of the icon. This parameter only has
            an effect in Windows where icon resources can contain several icons
            of different sizes.

        @see LoadFile()
    */
    wxIcon(const wxString& name, wxBitmapType type = wxICON_DEFAULT_TYPE,
           int desiredWidth = -1, int desiredHeight = -1);

    /**
        Loads an icon from the specified location.
    */
    wxIcon(const wxIconLocation& loc);

    /**
        Destructor.
        See @ref overview_refcount_destruct for more info.

        If the application omits to delete the icon explicitly, the icon will be
        destroyed automatically by wxWidgets when the application exits.

        @warning
        Do not delete an icon that is selected into a memory device context.
    */
    virtual ~wxIcon();

    /**
        Attach a Windows icon handle.

        This wxMSW-specific method allows assigning a native Windows @c HICON
        (which must be castes to @c WXHICON opaque handle type) to wxIcon.
        Notice that this means that the @c HICON will be destroyed by wxIcon
        when it is destroyed.

        @return @true if successful.

        @onlyfor{wxmsw}

        @since 2.9.5
    */
    bool CreateFromHICON(WXHICON icon);

    /**
        Returns disabled (dimmed) version of the icon.

        This method is available in wxIcon only under wxMSW, other ports only
        have it in wxBitmap. You can always use wxImage::ConvertToDisabled()
        and create the icon from wxImage manually however.

        @onlyfor{wxmsw}

        @since 2.9.0
    */
    wxIcon ConvertToDisabled(unsigned char brightness = 255) const;

    /**
        Copies @a bmp bitmap to this icon.
        Under MS Windows the bitmap must have mask colour set.

        @see LoadFile()
    */
    void CopyFromBitmap(const wxBitmap& bmp);

    /**
        Gets the colour depth of the icon.
        A value of 1 indicates a monochrome icon.
    */
    int GetDepth() const;

    /**
        Gets the height of the icon in pixels.

        @see GetWidth()
    */
    int GetHeight() const;

    /**
        Gets the width of the icon in pixels.

        @see GetHeight()
    */
    int GetWidth() const;

    /**
        Returns @true if icon data is present.
    */
    virtual bool IsOk() const;

    /**
        Loads an icon from a file or resource.

        @param name
            Either a filename or a Windows resource name.
            The meaning of name is determined by the @a type parameter.
        @param type
            One of the ::wxBitmapType values; see the note in the class
            detailed description.
            Note that the wxICON_DEFAULT_TYPE constant has different value under
            different wxWidgets ports. See the icon.h header for the value it takes
            for a specific port.
        @param desiredWidth
            Specifies the desired width of the icon. This parameter only has
            an effect in Windows where icon resources can contain several icons
            of different sizes.
        @param desiredHeight
            Specifies the desired height of the icon. This parameter only has
            an effect in Windows where icon resources can contain several icons
            of different sizes.

        @return @true if the operation succeeded, @false otherwise.
    */
    bool LoadFile(const wxString& name, wxBitmapType type = wxICON_DEFAULT_TYPE,
                  int desiredWidth = -1, int desiredHeight = -1);

    /**
        Sets the depth member (does not affect the icon data).

        @param depth
            Icon depth.
    */
    void SetDepth(int depth);

    /**
        Sets the height member (does not affect the icon data).

        @param height
            Icon height in pixels.
    */
    void SetHeight(int height);

    /**
        Sets the width member (does not affect the icon data).

        @param width
            Icon width in pixels.
    */
    void SetWidth(int width);

    /**
        Assignment operator, using @ref overview_refcount.

        @param icon
            Icon to assign.
    */
    wxIcon& operator=(const wxIcon& icon);
};

/**
    An empty wxIcon.
*/
wxIcon wxNullIcon;


