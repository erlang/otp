/////////////////////////////////////////////////////////////////////////////
// Name:        frame.h
// Purpose:     interface of wxFrame
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
   Frame specific styles
*/
#define wxFRAME_NO_TASKBAR      0x0002  // No taskbar button (MSW only)
#define wxFRAME_TOOL_WINDOW     0x0004  // No taskbar button, no system menu
#define wxFRAME_FLOAT_ON_PARENT 0x0008  // Always above its parent


/**
    @class wxFrame

    A frame is a window whose size and position can (usually) be changed by the user.

    It usually has thick borders and a title bar, and can optionally contain a
    menu bar, toolbar and status bar. A frame can contain any window that is not
    a frame or dialog.

    A frame that has a status bar and toolbar, created via the CreateStatusBar() and
    CreateToolBar() functions, manages these windows and adjusts the value returned
    by GetClientSize() to reflect the remaining size available to application windows.

    @remarks An application should normally define an wxCloseEvent handler for the
             frame to respond to system close events, for example so that related
             data and subwindows can be cleaned up.


    @section frame_defaultevent Default event processing

    wxFrame processes the following events:

    @li @c wxEVT_SIZE: if the frame has exactly one child window, not counting the
        status and toolbar, this child is resized to take the entire frame client area.
        If two or more windows are present, they should be laid out explicitly either
        by manually handling @c wxEVT_SIZE or using sizers;
    @li @c wxEVT_MENU_HIGHLIGHT: the default implementation displays the help string
        associated with the selected item in the first pane of the status bar, if there is one.


    @beginStyleTable
    @style{wxDEFAULT_FRAME_STYLE}
           Defined as wxMINIMIZE_BOX |  wxMAXIMIZE_BOX |  wxRESIZE_BORDER |
           wxSYSTEM_MENU |  wxCAPTION |  wxCLOSE_BOX |  wxCLIP_CHILDREN.
    @style{wxICONIZE}
           Display the frame iconized (minimized). Windows only.
    @style{wxCAPTION}
           Puts a caption on the frame. Notice that this flag is required by
           wxMINIMIZE_BOX, wxMAXIMIZE_BOX and wxCLOSE_BOX on most systems as
           the corresponding buttons cannot be shown if the window has no title
           bar at all. I.e. if wxCAPTION is not specified those styles would be
           simply ignored.
    @style{wxMINIMIZE}
           Identical to wxICONIZE. Windows only.
    @style{wxMINIMIZE_BOX}
           Displays a minimize box on the frame.
    @style{wxMAXIMIZE}
           Displays the frame maximized. Windows and GTK+ only.
    @style{wxMAXIMIZE_BOX}
           Displays a maximize box on the frame. Notice that under wxGTK
           wxRESIZE_BORDER must be used as well or this style is ignored.
    @style{wxCLOSE_BOX}
           Displays a close box on the frame.
    @style{wxSTAY_ON_TOP}
           Stay on top of all other windows, see also wxFRAME_FLOAT_ON_PARENT.
    @style{wxSYSTEM_MENU}
           Displays a system menu containing the list of various windows
           commands in the window title bar. Unlike wxMINIMIZE_BOX,
           wxMAXIMIZE_BOX and wxCLOSE_BOX styles this style can be used without
           wxCAPTION, at least under Windows, and makes the system menu
           available without showing it on screen in this case. However it is
           recommended to only use it together with wxCAPTION for consistent
           behaviour under all platforms.
    @style{wxRESIZE_BORDER}
           Displays a resizable border around the window.
    @style{wxFRAME_TOOL_WINDOW}
           Causes a frame with a small title bar to be created; the frame does
           not appear in the taskbar under Windows or GTK+.
    @style{wxFRAME_NO_TASKBAR}
           Creates an otherwise normal frame but it does not appear in the
           taskbar under Windows or GTK+ (note that it will minimize to the
           desktop window under Windows which may seem strange to the users
           and thus it might be better to use this style only without
           wxMINIMIZE_BOX style). In wxGTK, the flag is respected only if the
           window manager supports _NET_WM_STATE_SKIP_TASKBAR hint.
    @style{wxFRAME_FLOAT_ON_PARENT}
           The frame will always be on top of its parent (unlike wxSTAY_ON_TOP).
           A frame created with this style must have a non-@NULL parent.
    @style{wxFRAME_SHAPED}
           Windows with this style are allowed to have their shape changed
           with the SetShape() method.
    @endStyleTable

    The default frame style is for normal, resizable frames.
    To create a frame which cannot be resized by user, you may use the following
    combination of styles:

    @code
        wxDEFAULT_FRAME_STYLE & ~(wxRESIZE_BORDER | wxMAXIMIZE_BOX)
    @endcode

    See also the @ref overview_windowstyles.

    @beginExtraStyleTable
    @style{wxFRAME_EX_CONTEXTHELP}
           Under Windows, puts a query button on the caption. When pressed,
           Windows will go into a context-sensitive help mode and wxWidgets
           will send a @c wxEVT_HELP event if the user clicked on an application
           window. Note that this is an extended style and must be set by
           calling SetExtraStyle before Create is called (two-step
           construction). You cannot use this style together with
           wxMAXIMIZE_BOX or wxMINIMIZE_BOX, so you should use
           wxDEFAULT_FRAME_STYLE  ~ (wxMINIMIZE_BOX | wxMAXIMIZE_BOX) for the
           frames having this style (the dialogs don't have a minimize or a
           maximize box by default)
    @style{wxFRAME_EX_METAL}
           On macOS, frames with this style will be shown with a metallic
           look. This is an extra style.
    @endExtraStyleTable

    @beginEventEmissionTable
    @event{EVT_CLOSE(func)}
        Process a @c wxEVT_CLOSE_WINDOW event when the frame is being
        closed by the user or programmatically (see wxWindow::Close).
        The user may generate this event clicking the close button
        (typically the 'X' on the top-right of the title bar) if it's present
        (see the @c wxCLOSE_BOX style). See wxCloseEvent.
    @event{EVT_ICONIZE(func)}
        Process a @c wxEVT_ICONIZE event. See wxIconizeEvent.
    @event{EVT_MENU_OPEN(func)}
        A menu is about to be opened. See wxMenuEvent.
    @event{EVT_MENU_CLOSE(func)}
        A menu has been just closed. See wxMenuEvent.
    @event{EVT_MENU_HIGHLIGHT(id, func)}
        The menu item with the specified id has been highlighted: used to show
        help prompts in the status bar by wxFrame. See wxMenuEvent.
    @event{EVT_MENU_HIGHLIGHT_ALL(func)}
        A menu item has been highlighted, i.e. the currently selected menu item has changed.
        See wxMenuEvent.
    @endEventTable

    @library{wxcore}
    @category{managedwnd}

    @see wxMDIParentFrame, wxMDIChildFrame, wxMiniFrame, wxDialog
*/
class wxFrame : public wxTopLevelWindow
{
public:
    /**
        Default constructor.
    */
    wxFrame();

    /**
        Constructor, creating the window.

        @param parent
            The window parent. This may be, and often is, @NULL. If it is
            non-@NULL, the frame will be minimized when its parent is minimized
            and restored when it is restored (although it will still be
            possible to minimize and restore just this frame itself).
        @param id
            The window identifier. It may take a value of -1 to indicate a default value.
        @param title
            The caption to be displayed on the frame's title bar.
        @param pos
            The window position. The value wxDefaultPosition indicates a default position,
            chosen by either the windowing system or wxWidgets, depending on platform.
        @param size
            The window size. The value wxDefaultSize indicates a default size, chosen by
            either the windowing system or wxWidgets, depending on platform.
        @param style
            The window style. See wxFrame class description.
        @param name
            The name of the window. This parameter is used to associate a name with
            the item, allowing the application user to set Motif resource values for
            individual windows.

        @remarks For Motif, MWM (the Motif Window Manager) should be running for
                 any window styles to work (otherwise all styles take effect).

        @see Create()
    */
    wxFrame(wxWindow* parent, wxWindowID id,
            const wxString& title,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize,
            long style = wxDEFAULT_FRAME_STYLE,
            const wxString& name = wxFrameNameStr);

    /**
        Destructor. Destroys all child windows and menu bar if present.

        See @ref overview_windowdeletion for more info.
    */
    virtual ~wxFrame();

    /**
        Centres the frame on the display.

        @param direction
            The parameter may be wxHORIZONTAL, wxVERTICAL or wxBOTH.
    */
    void Centre(int direction = wxBOTH);

    /**
        Used in two-step frame construction.
        See wxFrame() for further details.
    */
    bool Create(wxWindow* parent, wxWindowID id, const wxString& title,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDEFAULT_FRAME_STYLE,
                const wxString& name = wxFrameNameStr);

    /**
        Creates a status bar at the bottom of the frame.

        @param number
            The number of fields to create. Specify a
            value greater than 1 to create a multi-field status bar.
        @param style
            The status bar style. See wxStatusBar for a list of valid styles.
        @param id
            The status bar window identifier. If -1, an identifier will be chosen
            by wxWidgets.
        @param name
            The status bar window name.

        @return A pointer to the status bar if it was created successfully, @NULL
                otherwise.

        @remarks The width of the status bar is the whole width of the frame
                 (adjusted automatically when resizing), and the height
                 and text size are chosen by the host windowing system.

        @see SetStatusText(), OnCreateStatusBar(), GetStatusBar()
    */
    virtual wxStatusBar* CreateStatusBar(int number = 1, long style = wxSTB_DEFAULT_STYLE,
                                         wxWindowID id = 0,
                                         const wxString& name = wxStatusBarNameStr);

    /**
        Creates a toolbar at the top or left of the frame.

        @param style
            The toolbar style. See wxToolBar for a list of valid styles.
        @param id
            The toolbar window identifier. If -1, an identifier will be chosen
            by wxWidgets.
        @param name
            The toolbar window name.

        @return A pointer to the toolbar if it was created successfully, @NULL
                otherwise.

        @remarks
            By default, the toolbar is an instance of wxToolBar.
            To use a different class, override OnCreateToolBar().
            When a toolbar has been created with this function, or made
            known to the frame with wxFrame::SetToolBar(), the frame will
            manage the toolbar position and adjust the return value from
            wxWindow::GetClientSize() to reflect the available space for
            application windows.
            Under Pocket PC, you should always use this function for creating
            the toolbar to be managed by the frame, so that wxWidgets can
            use a combined menubar and toolbar.
            Where you manage your own toolbars, create a wxToolBar as usual.

        @see CreateStatusBar(), OnCreateToolBar(), SetToolBar(), GetToolBar()
    */
    virtual wxToolBar* CreateToolBar(long style = wxTB_DEFAULT_STYLE,
                                     wxWindowID id = wxID_ANY,
                                     const wxString& name = wxToolBarNameStr);

    /**
        Method used to show help string of the selected menu toolbar item.

        This method is called by the default @c wxEVT_MENU_HIGHLIGHT event
        handler and also by wxToolBar to show the optional help string
        associated with the selected menu or toolbar item. It can be overridden
        if the default behaviour of showing this string in the frame status bar
        is not appropriate.

        @param text The help string to show, may be empty. The default
            implementation simply shows this string in the frame status bar
            (after remembering its previous text to restore it later).
        @param show Whether the help should be shown or hidden. The default
            implementation restores the previously saved status bar text when
            it is @false.

        @see SetStatusBarPane()
     */
    virtual void DoGiveHelp(const wxString& text, bool show);

    /**
        Returns the origin of the frame client area (in client coordinates).
        It may be different from (0, 0) if the frame has a toolbar.
    */
    virtual wxPoint GetClientAreaOrigin() const;

    /**
        Returns a pointer to the menubar currently associated with the frame (if any).

        @see SetMenuBar(), wxMenuBar, wxMenu
    */
    virtual wxMenuBar* GetMenuBar() const;

    /**
        Returns a pointer to the status bar currently associated with the frame
        (if any).

        @see CreateStatusBar(), wxStatusBar
    */
    virtual wxStatusBar* GetStatusBar() const;

    /**
        Returns the status bar pane used to display menu and toolbar help.

        @see SetStatusBarPane()
    */
    int GetStatusBarPane() const;

    /**
        Returns a pointer to the toolbar currently associated with the frame (if any).

        @see CreateToolBar(), wxToolBar, SetToolBar()
    */
    virtual wxToolBar* GetToolBar() const;

    /**
        Virtual function called when a status bar is requested by CreateStatusBar().

        @param number
            The number of fields to create.
        @param style
            The window style. See wxStatusBar for a list
            of valid styles.
        @param id
            The window identifier. If -1, an identifier will be chosen by
            wxWidgets.
        @param name
            The window name.

        @return A status bar object.

        @remarks An application can override this function to return a different
                 kind of status bar. The default implementation returns
                 an instance of wxStatusBar.

        @see CreateStatusBar(), wxStatusBar.
    */
    virtual wxStatusBar* OnCreateStatusBar(int number, long style,
                                           wxWindowID id,
                                           const wxString& name);

    /**
        Virtual function called when a toolbar is requested by CreateToolBar().

        @param style
            The toolbar style. See wxToolBar for a list
            of valid styles.
        @param id
            The toolbar window identifier. If -1, an identifier will be chosen by
            wxWidgets.
        @param name
            The toolbar window name.

        @return A toolbar object.

        @remarks An application can override this function to return a different
                 kind of toolbar. The default implementation returns an
                 instance of wxToolBar.

        @see CreateToolBar(), wxToolBar.
    */
    virtual wxToolBar* OnCreateToolBar(long style, wxWindowID id,
                                       const wxString& name);

    /**
        Simulate a menu command.

        @param id
            The identifier for a menu item.
    */
    bool ProcessCommand(int id);

    /**
        Tells the frame to show the given menu bar.

        @param menuBar
            The menu bar to associate with the frame.

        @remarks If the frame is destroyed, the menu bar and its menus will be
                 destroyed also, so do not delete the menu bar
                 explicitly (except by resetting the frame's menu bar to
                 another frame or @NULL).
                 Under Windows, a size event is generated, so be sure to
                 initialize data members properly before calling SetMenuBar().
                 Note that on some platforms, it is not possible to call this
                 function twice for the same frame object.

        @see GetMenuBar(), wxMenuBar, wxMenu.
    */
    virtual void SetMenuBar(wxMenuBar* menuBar);

    /**
        Associates a status bar with the frame.

        If @a statusBar is @NULL, then the status bar, if present, is detached from
        the frame, but @e not deleted.

        @see CreateStatusBar(), wxStatusBar, GetStatusBar()
    */
    virtual void SetStatusBar(wxStatusBar* statusBar);

    /**
        Set the status bar pane used to display menu and toolbar help.
        Using -1 disables help display.
    */
    void SetStatusBarPane(int n);

    /**
        Sets the status bar text and updates the status bar display.

        This is a simple wrapper for wxStatusBar::SetStatusText() which doesn't
        do anything if the frame has no status bar, i.e. GetStatusBar() returns
        @NULL.

        @param text
            The text for the status field.
        @param number
            The status field (starting from zero).

        @remarks Use an empty string to clear the status bar.

        @see CreateStatusBar(), wxStatusBar
    */
    virtual void SetStatusText(const wxString& text, int number = 0);

    /**
        Sets the widths of the fields in the status bar.

        @param n
            The number of fields in the status bar. It must be the
            same used in CreateStatusBar.
        @param widths_field
            Must contain an array of n integers, each of which is a status field width
            in pixels. A value of -1 indicates that the field is variable width; at
            least one field must be -1. You should delete this array after calling
            SetStatusWidths().

        @remarks The widths of the variable fields are calculated from the total
                 width of all fields, minus the sum of widths of the
                 non-variable fields, divided by the number of variable fields.

        @beginWxPerlOnly
        In wxPerl this method takes the field widths as parameters.
        @endWxPerlOnly
    */
    virtual void SetStatusWidths(int n, const int* widths_field);

    /**
        Associates a toolbar with the frame.
    */
    virtual void SetToolBar(wxToolBar* toolBar);

    /**
        MSW-specific function for accessing the taskbar button under Windows 7 or later.

        Returns a wxTaskBarButton pointer representing the taskbar button of the
        window under Windows 7 or later. The returned wxTaskBarButton may be
        used, if non-@c NULL, to access the functionality including thumbnail
        representations, thumbnail toolbars, notification and status overlays,
        and progress indicators.

        The returned pointer must @em not be deleted, it is owned by the frame
        and will be only deleted when the frame itself is destroyed.

        This function is not available in the other ports by design, any
        occurrences of it in the portable code must be guarded by
        @code #ifdef __WXMSW__ @endcode preprocessor guards.

        @since 3.1.0
    */
    wxTaskBarButton* MSWGetTaskBarButton();

    void PushStatusText(const wxString &text, int number = 0);
    void PopStatusText(int number = 0);

};

