/////////////////////////////////////////////////////////////////////////////
// Name:        toplevel.h
// Purpose:     interface of wxTopLevelWindow
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Styles used with wxTopLevelWindow::RequestUserAttention().
*/
enum
{
    wxUSER_ATTENTION_INFO = 1,  ///< Requests user attention,
    wxUSER_ATTENTION_ERROR = 2  ///< Results in a more drastic action.
};

/**
    Styles used with wxTopLevelWindow::ShowFullScreen().
*/
enum
{
    wxFULLSCREEN_NOMENUBAR   = 0x0001,  ///< Don't display the menu bar.
    wxFULLSCREEN_NOTOOLBAR   = 0x0002,  ///< Don't display toolbar bars.
    wxFULLSCREEN_NOSTATUSBAR = 0x0004,  ///< Don't display the status bar.
    wxFULLSCREEN_NOBORDER    = 0x0008,  ///< Don't display any border.
    wxFULLSCREEN_NOCAPTION   = 0x0010,  ///< Don't display a caption.

    /**
        Combination of all above, will display the least possible.
    */
    wxFULLSCREEN_ALL    = wxFULLSCREEN_NOMENUBAR | wxFULLSCREEN_NOTOOLBAR |
                          wxFULLSCREEN_NOSTATUSBAR | wxFULLSCREEN_NOBORDER |
                          wxFULLSCREEN_NOCAPTION
};

#define wxDEFAULT_FRAME_STYLE (wxSYSTEM_MENU |          \
                               wxRESIZE_BORDER |        \
                               wxMINIMIZE_BOX |         \
                               wxMAXIMIZE_BOX |         \
                               wxCLOSE_BOX |            \
                               wxCAPTION |              \
                               wxCLIP_CHILDREN)

/**
    @class wxTopLevelWindow

    wxTopLevelWindow is a common base class for wxDialog and wxFrame. It is an
    abstract base class meaning that you never work with objects of this class
    directly, but all of its methods are also applicable for the two classes
    above.

    Note that the instances of wxTopLevelWindow are managed by wxWidgets in the
    internal top level window list.

    @beginEventEmissionTable
    @event{EVT_MAXIMIZE(id, func)}
        Process a @c wxEVT_MAXIMIZE event. See wxMaximizeEvent.
    @event{EVT_MOVE(func)}
        Process a @c wxEVT_MOVE event, which is generated when a window is moved.
        See wxMoveEvent.
    @event{EVT_MOVE_START(func)}
        Process a @c wxEVT_MOVE_START event, which is generated when the user starts
        to move or size a window. wxMSW only.
        See wxMoveEvent.
    @event{EVT_MOVE_END(func)}
        Process a @c wxEVT_MOVE_END event, which is generated when the user stops
        moving or sizing a window. wxMSW only.
        See wxMoveEvent.
    @event{EVT_SHOW(func)}
        Process a @c wxEVT_SHOW event. See wxShowEvent.
    @endEventTable

    @library{wxcore}
    @category{managedwnd}

    @see wxDialog, wxFrame
*/
class wxTopLevelWindow : public wxNonOwnedWindow
{
public:
    /**
        Default ctor.
    */
    wxTopLevelWindow();

    /**
        Constructor creating the top level window.
    */
    wxTopLevelWindow(wxWindow *parent,
                    wxWindowID id,
                    const wxString& title,
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize,
                    long style = wxDEFAULT_FRAME_STYLE,
                    const wxString& name = wxFrameNameStr);

    /**
        Destructor. Remember that wxTopLevelWindows do not get immediately
        destroyed when the user (or the app) closes them; they have a
        @b delayed destruction.

        See @ref overview_windowdeletion for more info.
    */
    virtual ~wxTopLevelWindow();

    /**
        Creates the top level window.
    */
    bool Create(wxWindow *parent,
                wxWindowID id,
                const wxString& title,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDEFAULT_FRAME_STYLE,
                const wxString& name = wxFrameNameStr);

    /**
        Returns @true if the platform supports making the window translucent.

        @see SetTransparent()
    */
    virtual bool CanSetTransparent();

    /**
        A synonym for CentreOnScreen().
    */
    void CenterOnScreen(int direction = wxBOTH);

    /**
        Centres the window on screen.

        @param direction
            Specifies the direction for the centering. May be @c wxHORIZONTAL,
            @c wxVERTICAL or @c wxBOTH.

        @see wxWindow::CentreOnParent()
    */
    void CentreOnScreen(int direction = wxBOTH);

    /**
        Enables or disables the Close button (most often in the right upper
        corner of a dialog) and the Close entry of the system menu (most often
        in the left upper corner of the dialog).

        Returns @true if operation was successful. This may be wrong on X11
        (including GTK+) where the window manager may not support this operation
        and there is no way to find out.
    */
    virtual bool EnableCloseButton(bool enable = true);

    /**
        Enables or disables the Maximize button (in the right or left upper
        corner of a frame or dialog).

        Currently only implemented for wxMSW and wxOSX.

        The window style must contain wxMAXIMIZE_BOX.

        Returns @true if operation was successful. Note that a successful
        operation does not change the window style flags.

        @since 3.1.0
    */
    virtual bool EnableMaximizeButton(bool enable = true);

    /**
        Enables or disables the Minimize button (in the right or left upper
        corner of a frame or dialog).

        Currently only implemented for wxMSW and wxOSX.

        The window style must contain wxMINIMIZE_BOX.

        Note that in wxMSW a successful operation will change the window
        style flags.

        Returns @true if operation was successful. Note that a successful
        operation does not change the window style flags.

        @since 3.1.0
    */
    virtual bool EnableMinimizeButton(bool enable = true);

    /**
        Returns a pointer to the button which is the default for this window, or
        @c @NULL. The default button is the one activated by pressing the Enter
        key.
    */
    wxWindow* GetDefaultItem() const;

    /**
        Get the default size for a new top level window.

        This is used internally by wxWidgets on some platforms to determine the
        default size for a window created using ::wxDefaultSize so it is not
        necessary to use it when creating a wxTopLevelWindow, however it may be
        useful if a rough estimation of the window size is needed for some
        other reason.

        @since 2.9.2
     */
    static wxSize GetDefaultSize();

    /**
        Returns the standard icon of the window. The icon will be invalid if it
        hadn't been previously set by SetIcon().

        @see GetIcons()
    */
    wxIcon GetIcon() const;

    /**
        Returns all icons associated with the window, there will be none of them
        if neither SetIcon() nor SetIcons() had been called before. Use
        GetIcon() to get the main icon of the window.

        @see wxIconBundle
    */
    const wxIconBundle& GetIcons() const;

    /**
        Gets a string containing the window title.

        @see SetTitle()
    */
    virtual wxString GetTitle() const;

    /**
        Iconizes or restores the window.

        Note that in wxGTK the change to the window state is not immediate,
        i.e. IsIconized() will typically return @false right after a call to
        Iconize() and its return value will only change after the control flow
        returns to the event loop and the notification about the window being
        really iconized is received.

        @param iconize
            If @true, iconizes the window; if @false, shows and restores it.

        @see IsIconized(), Restore()(), wxIconizeEvent.
    */
    virtual void Iconize(bool iconize = true);

    /**
        Returns @true if this window is currently active, i.e.\ if the user is
        currently working with it.
    */
    virtual bool IsActive();

    /**
        Returns @true if this window is expected to be always maximized, either
        due to platform policy or due to local policy regarding particular
        class.
    */
    virtual bool IsAlwaysMaximized() const;

    /**
        Returns @true if the window is in fullscreen mode.

        @see ShowFullScreen()
    */
    virtual bool IsFullScreen() const;

    /**
        Returns @true if the window is iconized.
    */
    virtual bool IsIconized() const;

    /**
        Returns @true if the window is maximized.
    */
    virtual bool IsMaximized() const;

    /**
        This method is specific to wxUniversal port.

        Returns @true if this window is using native decorations, @false if we
        draw them ourselves.

        @see UseNativeDecorations(),
             UseNativeDecorationsByDefault()
    */
    bool IsUsingNativeDecorations() const;

    /**
        Lays out the children using the window sizer or resizes the only child
        of the window to cover its entire area.

        This class overrides the base class Layout() method to check if this
        window contains exactly one child -- which is commonly the case, with
        wxPanel being often created as the only child of wxTopLevelWindow --
        and, if this is the case, resizes this child window to cover the entire
        client area.

        Note that if you associate a sizer with this window, the sizer takes
        precedence and the only-child-resizing is only used as fallback.

        @returns @false if nothing was done because the window doesn't have
                 neither a sizer nor a single child, @true otherwise.
    */
    virtual bool Layout();

    /**
        Maximizes or restores the window.

        Note that, just as with Iconize(), the change to the window state is
        not immediate in at least wxGTK port.

        @param maximize
            If @true, maximizes the window, otherwise it restores it.

        @see Restore(), Iconize()
    */
    virtual void Maximize(bool maximize = true);

    /**
        MSW-specific function for accessing the system menu.

        Returns a wxMenu pointer representing the system menu of the window
        under MSW. The returned wxMenu may be used, if non-@c NULL, to add
        extra items to the system menu. The usual @c wxEVT_MENU
        events (that can be processed using @c EVT_MENU event table macro) will
        then be generated for them. All the other wxMenu methods may be used as
        well but notice that they won't allow you to access any standard system
        menu items (e.g. they can't be deleted or modified in any way
        currently).

        Notice that because of the native system limitations the identifiers of
        the items added to the system menu must be multiples of 16, otherwise
        no events will be generated for them.

        The returned pointer must @em not be deleted, it is owned by the window
        and will be only deleted when the window itself is destroyed.

        This function is not available in the other ports by design, any
        occurrences of it in the portable code must be guarded by
        @code #ifdef __WXMSW__ @endcode preprocessor guards.

        @since 2.9.3
     */
    wxMenu *MSWGetSystemMenu() const;

    /**
        Use a system-dependent way to attract users attention to the window when
        it is in background.

        @a flags may have the value of either @c ::wxUSER_ATTENTION_INFO
        (default) or @c ::wxUSER_ATTENTION_ERROR which results in a more drastic
        action. When in doubt, use the default value.


        @note This function should normally be only used when the application
              is not already in foreground.

        This function is currently implemented for Win32 where it flashes
        the window icon in the taskbar, and for wxGTK with task bars
        supporting it.

    */
    virtual void RequestUserAttention(int flags = wxUSER_ATTENTION_INFO);

    /**
        Restore a previously iconized or maximized window to its normal state.

        In wxGTK this method currently doesn't return the maximized window to
        its normal state and you must use Maximize() with @false argument
        explicitly for this. In the other ports, it both unmaximizes the
        maximized windows and uniconizes the iconized ones.

        @see Iconize(), Maximize()
     */
    void Restore();

    /**
        Class used with SaveGeometry() and RestoreToGeometry().

        This is an abstract base class, i.e. to use it you must define a
        derived class implementing the pure virtual SaveField() and
        RestoreField() methods.

        For example, if you wished to store the window geometry in a database,
        you could derive a class saving fields such as "width" or "height" in a
        table in this database and restoring them from it later.

        @since 3.1.2
     */
    class GeometrySerializer
    {
        /**
            Save a single field with the given value.

            Note that if this function returns @false, SaveGeometry() supposes
            that saving the geometry failed and returns @false itself, without
            even trying to save anything else.

            @param name uniquely identifies the field but is otherwise
                arbitrary.
            @param value value of the field (can be positive or negative, i.e.
                it can't be assumed that a value like -1 is invalid).

            @return @true if the field was saved or @false if saving it failed,
                resulting in wxTopLevelWindow::SaveGeometry() failure.
         */
        virtual bool SaveField(const wxString& name, int value) const = 0;

        /**
            Try to restore a single field.

            Unlike for SaveField(), returning @false from this function may
            indicate that the value simply wasn't present and doesn't prevent
            RestoreToGeometry() from continuing with trying to restore the
            other values.

            @param name uniquely identifies the field
            @param value non-@NULL pointer to the value to be filled by this
                function

            @return @true if the value was retrieved or @false if it wasn't
                found or an error occurred.
         */
        virtual bool RestoreField(const wxString& name, int* value) = 0;
    };

    /**
        Restores the window to the previously saved geometry.

        This is a companion function to SaveGeometry() and can be called later
        to restore the window to the geometry it had when it was saved.

        @param ser An object implementing GeometrySerializer virtual methods.

        @return @true if any (and, usually, but not necessarily, all) of the
            window geometry attributes were restored or @false if there was no
            saved geometry information at all or restoring it failed.

        @since 3.1.2
     */
    bool RestoreToGeometry(GeometrySerializer& ser);

    /**
        Save the current window geometry to allow restoring it later.

        After calling this function, window geometry is saved in the provided
        serializer and calling RestoreToGeometry() with the same serializer
        later (i.e. usually during a subsequent program execution) would
        restore the window to the same position, size, maximized/minimized
        state etc.

        This function is used by wxPersistentTLW, so it is not necessary to use
        it if the goal is to just save and restore window geometry in the
        simplest possible way. However is more flexibility is required, it can
        be also used directly with a custom serializer object.

        @param ser An object implementing GeometrySerializer virtual methods.

        @return @true if the geometry was saved, @false if doing it failed

        @since 3.1.2
     */
    bool SaveGeometry(const GeometrySerializer& ser) const;

    /**
        Changes the default item for the panel, usually @a win is a button.

        @see GetDefaultItem()
    */
    wxWindow* SetDefaultItem(wxWindow* win);


    wxWindow*  SetTmpDefaultItem(wxWindow * win);
    wxWindow* GetTmpDefaultItem() const;


    /**
        Sets the icon for this window.

        @param icon
            The wxIcon to associate with this window.

        @remarks The window takes a 'copy' of @a icon, but since it uses
                 reference counting, the copy is very quick. It is safe to
                 delete @a icon after calling this function.

        @note In wxMSW, @a icon must be either 16x16 or 32x32 icon.

        @see wxIcon, SetIcons()
    */
    void SetIcon(const wxIcon& icon);

    /**
        Sets several icons of different sizes for this window: this allows
        using different icons for different situations (e.g. task switching bar,
        taskbar, window title bar) instead of scaling, with possibly bad looking
        results, the only icon set by SetIcon().

        @param icons
            The icons to associate with this window.

        @note In wxMSW, @a icons must contain a 16x16 or 32x32 icon,
              preferably both.

        @see wxIconBundle
    */
    virtual void SetIcons(const wxIconBundle& icons);

    /**
        A simpler interface for setting the size hints than SetSizeHints().
    */
    virtual void SetMaxSize(const wxSize& size);

    /**
        A simpler interface for setting the size hints than SetSizeHints().
    */
    virtual void SetMinSize(const wxSize& size);

    /**
        Allows specification of minimum and maximum window sizes, and window
        size increments. If a pair of values is not set (or set to -1), no
        constraints will be used.

        @param minW
            The minimum width.
        @param minH
            The minimum height.
        @param maxW
            The maximum width.
        @param maxH
            The maximum height.
        @param incW
            Specifies the increment for sizing the width (GTK/Motif/Xt only).
        @param incH
            Specifies the increment for sizing the height (GTK/Motif/Xt only).

        @remarks Notice that this function not only prevents the user from
                 resizing the window outside the given bounds but it also
                 prevents the program itself from doing it using
                 wxWindow::SetSize().

    */
    virtual void SetSizeHints(int minW, int minH,
                              int maxW = -1, int maxH = -1,
                              int incW = -1, int incH = -1);

    /**
        Allows specification of minimum and maximum window sizes, and window
        size increments. If a pair of values is not set (or set to -1), no
        constraints will be used.

        @param minSize
            The minimum size of the window.
        @param maxSize
            The maximum size of the window.
        @param incSize
            Increment size (only taken into account under X11-based ports such
            as wxGTK/wxMotif/wxX11).

        @remarks Notice that this function not only prevents the user from
                 resizing the window outside the given bounds but it also
                 prevents the program itself from doing it using
                 wxWindow::SetSize().
    */
    void SetSizeHints(const wxSize& minSize,
                      const wxSize& maxSize = wxDefaultSize,
                      const wxSize& incSize = wxDefaultSize);

    /**
        Sets the window title.

        @param title
            The window title.

        @see GetTitle()
    */
    virtual void SetTitle(const wxString& title);

    /**
        If the platform supports it will set the window to be translucent.

        @param alpha
            Determines how opaque or transparent the window will be, if the
            platform supports the operation. A value of 0 sets the window to be
            fully transparent, and a value of 255 sets the window to be fully
            opaque.
    */
    virtual bool SetTransparent(wxByte alpha);

    /**
        This virtual function is not meant to be called directly but can be
        overridden to return @false (it returns @true by default) to allow the
        application to close even if this, presumably not very important, window
        is still opened. By default, the application stays alive as long as
        there are any open top level windows.
    */
    virtual bool ShouldPreventAppExit() const;

    /**
        This function sets the wxTopLevelWindow's modified state on macOS,
        which currently draws a black dot in the wxTopLevelWindow's close button.
        On other platforms, this method does nothing.

        @see OSXIsModified()
    */
    virtual void OSXSetModified(bool modified);

    /**
        Returns the current modified state of the wxTopLevelWindow on macOS.
        On other platforms, this method does nothing.

        @see OSXSetModified()
    */
    virtual bool OSXIsModified() const;

    /**
        Sets the file name represented by this wxTopLevelWindow.

        Under macOS, this file name is used to set the "proxy icon", which
        appears in the window title bar near its title, corresponding to this
        file name. Under other platforms it currently doesn't do anything but
        it is harmless to call it now and it might be implemented to do
        something useful in the future so you're encouraged to use it for any
        window representing a file-based document.

        @since 2.9.4
    */
    virtual void SetRepresentedFilename(const wxString& filename);

    /**
        Show the wxTopLevelWindow, but do not give it keyboard focus. This can be
        used for pop up or notification windows that should not steal the current
        focus.
     */
    virtual void ShowWithoutActivating();

    /**
        Enables the maximize button to toggle full screen mode. Prior to
        macOS 10.10 a full screen button is added to the right upper corner
        of a window's title bar.

        Currently only available for wxOSX/Cocoa.

        @param enable
            If @true (default) adds the full screen button in the title bar;
            if @false the button is removed.

        @return @true if the button was added or removed, @false if running
        under another OS.

        @note Having the button is also required to let ShowFullScreen()
        make use of the full screen API: a full screen window gets its own space
        and entering and exiting the mode is animated.
        If the button is not present the old way of switching to full screen
        is used.

        @onlyfor{wxosx}

        @see ShowFullScreen()

        @since 3.1.0
    */
    virtual bool EnableFullScreenView(bool enable = true);

    /**
        Depending on the value of @a show parameter the window is either shown
        full screen or restored to its normal state. @a style is a bit list
        containing some or all of the following values, which indicate what
        elements of the window to hide in full-screen mode:

        - @c ::wxFULLSCREEN_NOMENUBAR
        - @c ::wxFULLSCREEN_NOTOOLBAR
        - @c ::wxFULLSCREEN_NOSTATUSBAR
        - @c ::wxFULLSCREEN_NOBORDER
        - @c ::wxFULLSCREEN_NOCAPTION
        - @c ::wxFULLSCREEN_ALL (all of the above)

        This function has not been tested with MDI frames.

        @note Showing a window full screen also actually @ref wxWindow::Show()
              "Show()"s the window if it isn't shown.

        @see EnableFullScreenView(), IsFullScreen()
    */
    virtual bool ShowFullScreen(bool show, long style = wxFULLSCREEN_ALL);

    /**
        This method is specific to wxUniversal port.

        Use native or custom-drawn decorations for this window only. Notice that
        to have any effect this method must be called before really creating the
        window, i.e. two step creation must be used:

        @code
        MyFrame *frame = new MyFrame;       // use default ctor
        frame->UseNativeDecorations(false); // change from default "true"
        frame->Create(parent, title, ...);  // really create the frame
        @endcode

        @see UseNativeDecorationsByDefault(),
             IsUsingNativeDecorations()
    */
    void UseNativeDecorations(bool native = true);

    /**
        This method is specific to wxUniversal port.

        Top level windows in wxUniversal port can use either system-provided
        window decorations (i.e. title bar and various icons, buttons and menus
        in it) or draw the decorations themselves. By default the system
        decorations are used if they are available, but this method can be
        called with @a native set to @false to change this for all windows
        created after this point.

        Also note that if @c WXDECOR environment variable is set, then custom
        decorations are used by default and so it may make sense to call this
        method with default argument if the application can't use custom
        decorations at all for some reason.

        @see UseNativeDecorations()
    */
    void UseNativeDecorationsByDefault(bool native = true);
};

