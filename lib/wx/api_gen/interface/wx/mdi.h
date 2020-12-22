/////////////////////////////////////////////////////////////////////////////
// Name:        mdi.h
// Purpose:     interface of wxMDIClientWindow
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxMDIClientWindow

    An MDI client window is a child of wxMDIParentFrame, and manages zero or
    more wxMDIChildFrame objects.

    @remarks

    The client window is the area where MDI child windows exist. It doesn't have to
    cover the whole parent frame; other windows such as toolbars and a help window
    might coexist with it. There can be scrollbars on a client window, which are
    controlled by the parent window style.

    The wxMDIClientWindow class is usually adequate without further derivation, and
    it is created automatically when the MDI parent frame is created. If the application
    needs to derive a new class, the function wxMDIParentFrame::OnCreateClient() must
    be overridden in order to give an opportunity to use a different class of client
    window.

    Under wxMSW, the client window will automatically have a sunken border style
    when the active child is not maximized, and no border style when a child is maximized.

    @library{wxcore}
    @category{managedwnd}

    @see wxMDIChildFrame, wxMDIParentFrame, wxFrame
*/
class wxMDIClientWindow : public wxWindow
{
public:
    /**
        Default constructor.

        Objects of this class are only created by wxMDIParentFrame which uses
        the default constructor and calls CreateClient() immediately
        afterwards.
     */
    wxMDIClientWindow();

    /**
        Called by wxMDIParentFrame immediately after creating the client
        window.

        This function may be overridden in the derived class but the base class
        version must usually be called first to really create the window.

        @param parent
            The window parent.
        @param style
            The window style. Only wxHSCROLL and wxVSCROLL bits are meaningful
            here.

    */
    virtual bool CreateClient(wxMDIParentFrame* parent, long style = 0);
};



/**
    @class wxMDIParentFrame

    An MDI (Multiple Document Interface) parent frame is a window which can
    contain MDI child frames in its client area which emulates the full
    desktop.

    MDI is a user-interface model in which all the window reside inside the
    single parent window as opposed to being separate from each other. It
    remains popular despite dire warnings from Microsoft itself (which
    popularized this model in the first model) that MDI is obsolete.

    An MDI parent frame always has a wxMDIClientWindow associated with it,
    which is the parent for MDI child frames. In the simplest case, the client
    window takes up the entire parent frame area but it is also possible to
    resize it to be smaller in order to have other windows in the frame, a
    typical example is using a sidebar along one of the window edges.

    The appearance of MDI applications differs between different ports. The
    classic MDI model, with child windows which can be independently moved,
    resized etc, is only available under MSW, which provides native support for
    it. In Mac ports, multiple top level windows are used for the MDI children
    too and the MDI parent frame itself is invisible, to accommodate the native
    look and feel requirements. In all the other ports, a tab-based MDI
    implementation (sometimes called TDI) is used and so at most one MDI child
    is visible at any moment (child frames are always maximized).

    @remarks

    Although it is possible to have multiple MDI parent frames, a typical MDI
    application has a single MDI parent frame window inside which multiple MDI
    child frames, i.e. objects of class wxMDIChildFrame, can be created.


    @beginStyleTable

    There are no special styles for this class, all wxFrame styles apply to it
    in the usual way. The only exception is that wxHSCROLL and wxVSCROLL styles
    apply not to the frame itself but to the client window, so that using them
    enables horizontal and vertical scrollbars for this window and not the
    frame.

    @endStyleTable

    @library{wxcore}
    @category{managedwnd}

    @see wxMDIChildFrame, wxMDIClientWindow, wxFrame, wxDialog
*/
class wxMDIParentFrame : public wxFrame
{
public:

    /**
        Default constructor.

        Use Create() for the objects created using this constructor.
    */
    wxMDIParentFrame();

    /**
        Constructor, creating the window.

        Notice that if you override virtual OnCreateClient() method you
        shouldn't be using this constructor but the default constructor and
        Create() as otherwise your overridden method is never going to be
        called because of the usual C++ virtual call resolution rules.

        @param parent
            The window parent. Usually is @NULL.
        @param id
            The window identifier. It may take a value of @c wxID_ANY to
            indicate a default value.
        @param title
            The caption to be displayed on the frame's title bar.
        @param pos
            The window position. The value ::wxDefaultPosition indicates a
            default position, chosen by either the windowing system or
            wxWidgets, depending on platform.
        @param size
            The window size. The value ::wxDefaultSize indicates a default
            size, chosen by either the windowing system or wxWidgets, depending
            on platform.
        @param style
            The window style. Default value includes wxHSCROLL and wxVSCROLL
            styles.
        @param name
            The name of the window. This parameter is used to associate a name
            with the item, allowing the application user to set Motif resource
            values for individual windows.

        @remarks

        Under wxMSW, the client window will automatically have a sunken
        border style when the active child is not maximized, and no border
        style when a child is maximized.

        @see Create(), OnCreateClient()
    */
    wxMDIParentFrame(wxWindow* parent, wxWindowID id,
                     const wxString& title,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     long style = wxDEFAULT_FRAME_STYLE | wxVSCROLL | wxHSCROLL,
                     const wxString& name = wxFrameNameStr);

    /**
        Destructor.

        Destroys all child windows and menu bar if present.
    */
    virtual ~wxMDIParentFrame();

    /**
        Activates the MDI child following the currently active one.

        The MDI children are maintained in an ordered list and this function
        switches to the next element in this list, wrapping around the end of
        it if the currently active child is the last one.

        @see ActivatePrevious()
    */
    virtual void ActivateNext();

    /**
        Activates the MDI child preceding the currently active one.

        @see ActivateNext()
    */
    virtual void ActivatePrevious();

    /**
        Arranges any iconized (minimized) MDI child windows.

        This method is only implemented in MSW MDI implementation and does
        nothing under the other platforms.

        @see Cascade(), Tile()
    */
    virtual void ArrangeIcons();

    /**
        Arranges the MDI child windows in a cascade.

        This method is only implemented in MSW MDI implementation and does
        nothing under the other platforms.

        @see Tile(), ArrangeIcons()
    */
    virtual void Cascade();

    /**
        Used in two-step frame construction.

        See wxMDIParentFrame() for further details.
    */
    bool Create(wxWindow* parent,
                wxWindowID id,
                const wxString& title,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDEFAULT_FRAME_STYLE | wxVSCROLL | wxHSCROLL,
                const wxString& name = wxFrameNameStr);

    /**
        Returns a pointer to the active MDI child, if there is one.

        If there are any children at all this function returns a non-@NULL
        pointer.
    */
    virtual wxMDIChildFrame* GetActiveChild() const;

    /**
        Returns a pointer to the client window.

        @see OnCreateClient()
    */
    wxMDIClientWindowBase* GetClientWindow() const;

    /**
        Returns the current MDI Window menu.

        Unless wxFRAME_NO_WINDOW_MENU style was used, a default menu listing
        all the currently active children and providing the usual operations
        (tile, cascade, ...) on them is created automatically by the library
        and this function can be used to retrieve it. Notice that the default
        menu can be replaced by calling SetWindowMenu().

        This function is currently not available under macOS.

        @return The current Window menu or @NULL.
    */
    wxMenu *GetWindowMenu() const;

    /**
        Returns whether the MDI implementation is tab-based.

        Currently only the MSW port uses the real MDI. In Mac ports the usual
        SDI is used, as common under this platforms, and all the other ports
        use TDI implementation.

        TDI-based MDI applications have different appearance and functionality
        (e.g. child frames can't be minimized and only one of them is visible
        at any given time) so the application may need to adapt its interface
        somewhat depending on the return value of this function.
     */
    static bool IsTDI();

    /**
        Override this to return a different kind of client window.

        If you override this function, you must create your parent frame in two
        stages, or your function will never be called, due to the way C++
        treats virtual functions called from constructors. For example:

        @code
        frame = new MyParentFrame;
        frame->Create(parent, myParentFrameId, "My Parent Frame");
        @endcode

        @remarks

        You might wish to derive from wxMDIClientWindow in order to implement
        different erase behaviour, for example, such as painting a bitmap on
        the background.

        Note that it is probably impossible to have a client window that scrolls
        as well as painting a bitmap or pattern, since in @b OnScroll, the scrollbar
        positions always return zero.

        @see GetClientWindow(), wxMDIClientWindow
    */
    virtual wxMDIClientWindow* OnCreateClient();

    /**
        Replace the current MDI Window menu.

        Ownership of the menu object passes to the frame when you call this
        function, i.e. the menu will be deleted by it when it's no longer
        needed (usually when the frame itself is deleted or when
        SetWindowMenu() is called again).

        To remove the window completely, you can use the wxFRAME_NO_WINDOW_MENU
        window style but this function also allows doing it by passing @NULL
        pointer as @a menu.

        The menu may include the items with the following standard identifiers
        (but may use arbitrary text and help strings and bitmaps for them):
            - @c wxID_MDI_WINDOW_CASCADE
            - @c wxID_MDI_WINDOW_TILE_HORZ
            - @c wxID_MDI_WINDOW_TILE_VERT
            - @c wxID_MDI_WINDOW_ARRANGE_ICONS
            - @c wxID_MDI_WINDOW_PREV
            - @c wxID_MDI_WINDOW_NEXT
        All of which are handled by wxMDIParentFrame itself. If any other
        commands are used in the menu, the derived frame should handle them.

        This function is currently not available under macOS.

        @param menu
            The menu to be used instead of the standard MDI Window menu or @NULL.
    */
    virtual void SetWindowMenu(wxMenu* menu);

    /**
        Tiles the MDI child windows either horizontally or vertically depending
        on whether @a orient is @c wxHORIZONTAL or @c wxVERTICAL.

        This method is only implemented in MSW MDI implementation and does
        nothing under the other platforms.

    */
    virtual void Tile(wxOrientation orient = wxHORIZONTAL);
};



/**
    @class wxMDIChildFrame

    An MDI child frame is a frame that can only exist inside a
    wxMDIClientWindow, which is itself a child of wxMDIParentFrame.

    @beginStyleTable
    All of the standard wxFrame styles can be used but most of them are ignored
    by TDI-based MDI implementations.
    @endStyleTable

    @remarks
    Although internally an MDI child frame is a child of the MDI client window,
    in wxWidgets you create it as a child of wxMDIParentFrame. In fact, you can
    usually forget that the client window exists. MDI child frames are clipped
    to the area of the MDI client window, and may be iconized on the client
    window. You can associate a menubar with a child frame as usual, although
    an MDI child doesn't display its menubar under its own title bar. The MDI
    parent frame's menubar will be changed to reflect the currently active
    child frame. If there are currently no children, the parent frame's own
    menubar will be displayed.

    @library{wxcore}
    @category{managedwnd}

    @see wxMDIClientWindow, wxMDIParentFrame, wxFrame
*/
class wxMDIChildFrame : public wxFrame
{
public:
    /**
        Default constructor.
    */
    wxMDIChildFrame();

    /**
        Constructor, creating the window.

        @param parent
            The window parent. This should not be @NULL.
        @param id
            The window identifier. It may take a value of -1 to indicate a default
            value.
        @param title
            The caption to be displayed on the frame's title bar.
        @param pos
            The window position. The value ::wxDefaultPosition indicates a default position,
            chosen by either the windowing system or wxWidgets, depending on platform.
        @param size
            The window size. The value ::wxDefaultSize indicates a default size, chosen by
            either the windowing system or wxWidgets, depending on platform.
        @param style
            The window style. See wxMDIChildFrame.
        @param name
            The name of the window. This parameter is used to associate a name with the
            item, allowing the application user to set Motif resource values for individual
            windows.

        @see Create()
    */
    wxMDIChildFrame(wxMDIParentFrame* parent, wxWindowID id,
                    const wxString& title,
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize,
                    long style = wxDEFAULT_FRAME_STYLE,
                    const wxString& name = wxFrameNameStr);

    /**
        Destructor. Destroys all child windows and menu bar if present.
    */
    virtual ~wxMDIChildFrame();

    /**
        Activates this MDI child frame.

        @see Maximize(), Restore()
    */
    virtual void Activate();

    /**
        Used in two-step frame construction.
        See wxMDIChildFrame() for further details.
    */
    bool Create(wxMDIParentFrame* parent, wxWindowID id, const wxString& title,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDEFAULT_FRAME_STYLE,
                const wxString& name = wxFrameNameStr);

    /**
        Returns the MDI parent frame containing this child.

        Notice that this may return a different object than GetParent() as the
        child frames may be created as children of the client window
        internally.
     */
    wxMDIParentFrame *GetMDIParent() const;

    /**
        Returns true for MDI children in TDI implementations.

        TDI-based implementations represent MDI children as pages in a
        wxNotebook and so they are always maximized and can't be restored or
        iconized.

        @see wxMDIParentFrame::IsTDI().
     */
    virtual bool IsAlwaysMaximized() const;

    /**
        Maximizes this MDI child frame.

        This function doesn't do anything if IsAlwaysMaximized() returns @true.

        @see Activate(), Restore()
    */
    virtual void Maximize(bool maximize = true);

    /**
        Restores this MDI child frame (unmaximizes).

        This function doesn't do anything if IsAlwaysMaximized() returns @true.

        @see Activate(), Maximize()
    */
    virtual void Restore();
};

