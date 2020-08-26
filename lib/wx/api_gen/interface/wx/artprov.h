/////////////////////////////////////////////////////////////////////////////
// Name:        artprov.h
// Purpose:     interface of wxArtProvider
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    This type identifies the client of the art objects requested to wxArtProvider.
*/
typedef wxString wxArtClient;

/**
    This type identifies a specific art object which can be requested to wxArtProvider.
*/
typedef wxString wxArtID;


wxArtClient wxART_TOOLBAR;
wxArtClient wxART_MENU;
wxArtClient wxART_FRAME_ICON;

wxArtClient wxART_CMN_DIALOG;
wxArtClient wxART_HELP_BROWSER;
wxArtClient wxART_MESSAGE_BOX;
wxArtClient wxART_BUTTON;
wxArtClient wxART_LIST;

wxArtClient wxART_OTHER;


wxArtID wxART_ADD_BOOKMARK;
wxArtID wxART_DEL_BOOKMARK;
wxArtID wxART_HELP_SIDE_PANEL;
wxArtID wxART_HELP_SETTINGS;
wxArtID wxART_HELP_BOOK;
wxArtID wxART_HELP_FOLDER;
wxArtID wxART_HELP_PAGE;
wxArtID wxART_GO_BACK;
wxArtID wxART_GO_FORWARD;
wxArtID wxART_GO_UP;
wxArtID wxART_GO_DOWN;
wxArtID wxART_GO_TO_PARENT;
wxArtID wxART_GO_HOME;
wxArtID wxART_GOTO_FIRST;
wxArtID wxART_GOTO_LAST;
wxArtID wxART_FILE_OPEN;
wxArtID wxART_FILE_SAVE;
wxArtID wxART_FILE_SAVE_AS;
wxArtID wxART_PRINT;
wxArtID wxART_HELP;
wxArtID wxART_TIP;
wxArtID wxART_REPORT_VIEW;
wxArtID wxART_LIST_VIEW;
wxArtID wxART_NEW_DIR;
wxArtID wxART_HARDDISK;
wxArtID wxART_FLOPPY;
wxArtID wxART_CDROM;
wxArtID wxART_REMOVABLE;
wxArtID wxART_FOLDER;
wxArtID wxART_FOLDER_OPEN;
wxArtID wxART_GO_DIR_UP;
wxArtID wxART_EXECUTABLE_FILE;
wxArtID wxART_NORMAL_FILE;
wxArtID wxART_TICK_MARK;
wxArtID wxART_CROSS_MARK;
wxArtID wxART_ERROR;
wxArtID wxART_QUESTION;
wxArtID wxART_WARNING;
wxArtID wxART_INFORMATION;
wxArtID wxART_MISSING_IMAGE;

wxArtID wxART_COPY;
wxArtID wxART_CUT;
wxArtID wxART_PASTE;
wxArtID wxART_DELETE;
wxArtID wxART_NEW;

wxArtID wxART_UNDO;
wxArtID wxART_REDO;

wxArtID wxART_PLUS;
wxArtID wxART_MINUS;

wxArtID wxART_CLOSE;
wxArtID wxART_QUIT;

wxArtID wxART_FIND;
wxArtID wxART_FIND_AND_REPLACE;

wxArtID wxART_FULL_SCREEN;
wxArtID wxART_EDIT;


/**
    @class wxArtProvider

    wxArtProvider class is used to customize the look of wxWidgets application.

    When wxWidgets needs to display an icon or a bitmap (e.g. in the standard file
    dialog), it does not use a hard-coded resource but asks wxArtProvider for it
    instead. This way users can plug in their own wxArtProvider class and easily
    replace standard art with their own version.

    All that is needed is to derive a class from wxArtProvider, override either its
    wxArtProvider::CreateBitmap() and/or its wxArtProvider::CreateIconBundle() methods
    and register the provider with wxArtProvider::Push():

    @code
      class MyProvider : public wxArtProvider
      {
      protected:
        wxBitmap CreateBitmap(const wxArtID& id,
                              const wxArtClient& client,
                              const wxSize size)

        // optionally override this one as well
        wxIconBundle CreateIconBundle(const wxArtID& id,
                                      const wxArtClient& client)
        { ... }
      };
      ...
      wxArtProvider::Push(new MyProvider);
    @endcode

    If you need bitmap images (of the same artwork) that should be displayed at
    different sizes you should probably consider overriding wxArtProvider::CreateIconBundle
    and supplying icon bundles that contain different bitmap sizes.

    There's another way of taking advantage of this class: you can use it in your
    code and use platform native icons as provided by wxArtProvider::GetBitmap or
    wxArtProvider::GetIcon.

    @section artprovider_identify Identifying art resources

    Every bitmap and icon bundle are known to wxArtProvider under an unique ID that
    is used when requesting a resource from it. The ID is represented by the ::wxArtID type
    and can have one of these predefined values (you can see bitmaps represented by these
    constants in the @ref page_samples_artprov):

    <table>
    <tr><td>
     @li @c wxART_ERROR
     @li @c wxART_QUESTION
     @li @c wxART_WARNING
     @li @c wxART_INFORMATION
     @li @c wxART_ADD_BOOKMARK
     @li @c wxART_DEL_BOOKMARK
     @li @c wxART_HELP_SIDE_PANEL
     @li @c wxART_HELP_SETTINGS
     @li @c wxART_HELP_BOOK
     @li @c wxART_HELP_FOLDER
     @li @c wxART_HELP_PAGE
     @li @c wxART_GO_BACK
     @li @c wxART_GO_FORWARD
     @li @c wxART_GO_UP
     @li @c wxART_GO_DOWN
     @li @c wxART_GO_TO_PARENT
     @li @c wxART_GO_HOME
     @li @c wxART_GOTO_FIRST (since 2.9.2)
     </td><td>
     @li @c wxART_GOTO_LAST (since 2.9.2)
     @li @c wxART_PRINT
     @li @c wxART_HELP
     @li @c wxART_TIP
     @li @c wxART_REPORT_VIEW
     @li @c wxART_LIST_VIEW
     @li @c wxART_NEW_DIR
     @li @c wxART_FOLDER
     @li @c wxART_FOLDER_OPEN
     @li @c wxART_GO_DIR_UP
     @li @c wxART_EXECUTABLE_FILE
     @li @c wxART_NORMAL_FILE
     @li @c wxART_TICK_MARK
     @li @c wxART_CROSS_MARK
     @li @c wxART_MISSING_IMAGE
     @li @c wxART_NEW
     @li @c wxART_FILE_OPEN
     @li @c wxART_FILE_SAVE
     </td><td>
     @li @c wxART_FILE_SAVE_AS
     @li @c wxART_DELETE
     @li @c wxART_COPY
     @li @c wxART_CUT
     @li @c wxART_PASTE
     @li @c wxART_UNDO
     @li @c wxART_REDO
     @li @c wxART_PLUS (since 2.9.2)
     @li @c wxART_MINUS (since 2.9.2)
     @li @c wxART_CLOSE
     @li @c wxART_QUIT
     @li @c wxART_FIND
     @li @c wxART_FIND_AND_REPLACE
     @li @c wxART_FULL_SCREEN (since 3.1.0)
     @li @c wxART_EDIT (since 3.1.0)
     @li @c wxART_HARDDISK
     @li @c wxART_FLOPPY
     @li @c wxART_CDROM
     @li @c wxART_REMOVABLE
    </td></tr>
    </table>

    Additionally, any string recognized by custom art providers registered using
    wxArtProvider::Push may be used.

    @note
    When running under GTK+ 2, GTK+ stock item IDs (e.g. @c "gtk-cdrom") may be used
    as well:
    @code
    #ifdef __WXGTK__
        wxBitmap bmp = wxArtProvider::GetBitmap("gtk-cdrom", wxART_MENU);
    #endif
    @endcode
    For a list of the GTK+ stock items please refer to the
    <a href="http://library.gnome.org/devel/gtk/stable/gtk-Stock-Items.html">GTK+ documentation
    page</a>.
    It is also possible to load icons from the current icon theme by specifying their name
    (without extension and directory components).
    Icon themes recognized by GTK+ follow the freedesktop.org
    <a href="http://freedesktop.org/Standards/icon-theme-spec">Icon Themes specification</a>.
    Note that themes are not guaranteed to contain all icons, so wxArtProvider may
    return ::wxNullBitmap or ::wxNullIcon.
    The default theme is typically installed in @c /usr/share/icons/hicolor.


    @section artprovider_clients Clients

    The @e client is the entity that calls wxArtProvider's GetBitmap() or GetIcon() function.
    It is represented by wxClientID type and can have one of these values:

    @li @c wxART_TOOLBAR
    @li @c wxART_MENU
    @li @c wxART_BUTTON
    @li @c wxART_FRAME_ICON
    @li @c wxART_CMN_DIALOG
    @li @c wxART_HELP_BROWSER
    @li @c wxART_MESSAGE_BOX
    @li @c wxART_OTHER (used for all requests that don't fit into any of the
        categories above)

    Client ID serve as a hint to wxArtProvider that is supposed to help it to
    choose the best looking bitmap. For example it is often desirable to use
    slightly different icons in menus and toolbars even though they represent
    the same action (e.g. wxART_FILE_OPEN). Remember that this is really only a
    hint for wxArtProvider -- it is common that wxArtProvider::GetBitmap returns
    identical bitmap for different client values!

    @library{wxcore}
    @category{misc}

    @see @ref page_samples_artprov for an example of wxArtProvider usage;
         @ref page_stockitems "stock ID list"
*/
class wxArtProvider : public wxObject
{
public:
    /**
        The destructor automatically removes the provider from the provider stack used
        by GetBitmap().
    */
    virtual ~wxArtProvider();

    /**
        Delete the given @a provider.
    */
    static bool Delete(wxArtProvider* provider);

    /**
        Query registered providers for bitmap with given ID.

        @param id
            wxArtID unique identifier of the bitmap.
        @param client
            wxArtClient identifier of the client (i.e. who is asking for the bitmap).
        @param size
            Size of the returned bitmap or wxDefaultSize if size doesn't matter.

        @return The bitmap if one of registered providers recognizes the ID or
                wxNullBitmap otherwise.
    */
    static wxBitmap GetBitmap(const wxArtID& id,
                              const wxArtClient& client = wxART_OTHER,
                              const wxSize& size = wxDefaultSize);

    /**
        Same as wxArtProvider::GetBitmap, but return a wxIcon object
        (or ::wxNullIcon on failure).
    */
    static wxIcon GetIcon(const wxArtID& id,
                          const wxArtClient& client = wxART_OTHER,
                          const wxSize& size = wxDefaultSize);

    /**
        Returns native icon size for use specified by @a client hint.

        If the platform has no commonly used default for this use or if
        @a client is not recognized, returns wxDefaultSize.

        @note In some cases, a platform may have @em several appropriate
              native sizes (for example, wxART_FRAME_ICON for frame icons).
              In that case, this method returns only one of them, picked
              reasonably.

        @since 2.9.0
     */
    static wxSize GetNativeSizeHint(const wxArtClient& client);

    /**
        Returns a suitable size hint for the given @e wxArtClient.

        If @a platform_default is @true, return a size based on the current
        platform using GetNativeSizeHint(), otherwise return the size from the
        topmost wxArtProvider. @e wxDefaultSize may be returned if the client
        doesn't have a specified size, like wxART_OTHER for example.

        @see GetNativeSizeHint()
    */
    static wxSize GetSizeHint(const wxArtClient& client,
                              bool platform_default = false);

    /**
        Query registered providers for icon bundle with given ID.

        @param id
            wxArtID unique identifier of the icon bundle.
        @param client
            wxArtClient identifier of the client (i.e. who is asking for the icon
            bundle).

        @return The icon bundle if one of registered providers recognizes the ID
                or wxNullIconBundle otherwise.
    */
    static wxIconBundle GetIconBundle(const wxArtID& id,
                                      const wxArtClient& client = wxART_OTHER);

    /**
        Returns true if the platform uses native icons provider that should
        take precedence over any customizations.

        This is true for any platform that has user-customizable icon themes,
        currently only wxGTK.

        A typical use for this method is to decide whether a custom art provider
        should be plugged in using Push() or PushBack().

        @since 2.9.0
     */
    static bool HasNativeProvider();

    /**
        @deprecated Use PushBack() instead.
    */
    static void Insert(wxArtProvider* provider);

    /**
        Remove latest added provider and delete it.
    */
    static bool Pop();

    /**
        Register new art provider and add it to the top of providers stack
        (i.e. it will be queried as the first provider).

        @see PushBack()
    */
    static void Push(wxArtProvider* provider);

    /**
        Register new art provider and add it to the bottom of providers stack.
        In other words, it will be queried as the last one, after all others,
        including the default provider.

        @see Push()

        @since 2.9.0
    */
    static void PushBack(wxArtProvider* provider);

    /**
        Remove a provider from the stack if it is on it. The provider is not
        deleted, unlike when using Delete().
    */
    static bool Remove(wxArtProvider* provider);

    /**
     * Helper used by GetMessageBoxIcon(): return the art id corresponding to
     * the standard wxICON_INFORMATION/WARNING/ERROR/QUESTION flags (only one
     * can be set)
     */
    static wxArtID GetMessageBoxIconId(int flags);

    /**
     * Helper used by several generic classes: return the icon corresponding to
     * the standard wxICON_INFORMATION/WARNING/ERROR/QUESTION flags (only one
     * can be set)
     */
    static wxIcon GetMessageBoxIcon(int flags);


protected:

    /**
        Derived art provider classes must override this method to create requested art
        resource. Note that returned bitmaps are cached by wxArtProvider and it is
        therefore not necessary to optimize CreateBitmap() for speed (e.g. you may
        create wxBitmap objects from XPMs here).

        @param id
            wxArtID unique identifier of the bitmap.
        @param client
            wxArtClient identifier of the client (i.e. who is asking for the bitmap).
            This only serves as a hint.
        @param size
            Preferred size of the bitmap. The function may return a bitmap of different
            dimensions, it will be automatically rescaled to meet client's request.

        @note
        This is not part of wxArtProvider's public API, use wxArtProvider::GetBitmap
        or wxArtProvider::GetIconBundle or wxArtProvider::GetIcon to query wxArtProvider
        for a resource.

        @see CreateIconBundle()
    */
    virtual wxBitmap CreateBitmap(const wxArtID& id,
                                  const wxArtClient& client,
                                  const wxSize& size);

    /**
        This method is similar to CreateBitmap() but can be used when a bitmap
        (or an icon) exists in several sizes.
    */
    virtual wxIconBundle CreateIconBundle(const wxArtID& id,
                                          const wxArtClient& client);
};

