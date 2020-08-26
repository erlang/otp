/////////////////////////////////////////////////////////////////////////////
// Name:        window.h
// Purpose:     interface of wxWindow
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    Valid values for wxWindow::ShowWithEffect() and wxWindow::HideWithEffect().
*/
enum wxShowEffect
{
    /**
        No effect, equivalent to normal wxWindow::Show() or Hide() call.

        @since 2.9.1
     */
    wxSHOW_EFFECT_NONE,

    /// Roll window to the left
    wxSHOW_EFFECT_ROLL_TO_LEFT,

    /// Roll window to the right
    wxSHOW_EFFECT_ROLL_TO_RIGHT,

    /// Roll window to the top
    wxSHOW_EFFECT_ROLL_TO_TOP,

    /// Roll window to the bottom
    wxSHOW_EFFECT_ROLL_TO_BOTTOM,

    /// Slide window to the left
    wxSHOW_EFFECT_SLIDE_TO_LEFT,

    /// Slide window to the right
    wxSHOW_EFFECT_SLIDE_TO_RIGHT,

    /// Slide window to the top
    wxSHOW_EFFECT_SLIDE_TO_TOP,

    /// Slide window to the bottom
    wxSHOW_EFFECT_SLIDE_TO_BOTTOM,

    /// Fade in or out effect
    wxSHOW_EFFECT_BLEND,

    /// Expanding or collapsing effect
    wxSHOW_EFFECT_EXPAND,

    wxSHOW_EFFECT_MAX
};


/**
    Values for wxWindow::EnableTouchEvents() mask.

    The values other than ::wxTOUCH_NONE and ::wxTOUCH_ALL_GESTURES can be
    combined together to request enabling events for the specified gestures and
    for them only.

    @since 3.1.1
 */
enum
{
    /**
        Don't generate any touch events.
     */
    wxTOUCH_NONE,

    /**
        Generate wxPanGestureEvent for vertical pans.

        Note that under macOS horizontal pan events are also enabled when this
        flag is specified.
     */
    wxTOUCH_VERTICAL_PAN_GESTURE,

    /**
        Generate wxPanGestureEvent for horizontal pans.

        Note that under macOS vertical pan events are also enabled when this
        flag is specified.
     */
    wxTOUCH_HORIZONTAL_PAN_GESTURE,

    /**
        Generate wxPanGestureEvent for any pans.

        This is just a convenient combination of wxTOUCH_VERTICAL_PAN_GESTURE
        and wxTOUCH_HORIZONTAL_PAN_GESTURE.
     */
    wxTOUCH_PAN_GESTURES,

    /**
        Generate wxZoomGestureEvent.
     */
    wxTOUCH_ZOOM_GESTURE,

    /**
        Generate wxRotateGestureEvent.
     */
    wxTOUCH_ROTATE_GESTURE,

    /**
        Generate events for press or tap gestures such as wxTwoFingerTapEvent,
        wxLongPressEvent and wxPressAndTapEvent.
     */
    wxTOUCH_PRESS_GESTURES,

    /**
        Enable all supported gesture events.
     */
    wxTOUCH_ALL_GESTURES
};

/**
   flags for SendSizeEvent()
*/
enum
{
    wxSEND_EVENT_POST = 1
};




/**
    Struct containing all the visual attributes of a control.
*/
struct  wxVisualAttributes
{
    /// The font used for control label/text inside it.
    wxFont font;

    /// The foreground colour.
    wxColour colFg;

    /**
        The background colour.

        May be wxNullColour if the controls background colour is not solid.
     */
    wxColour colBg;
};


/**
    Different window variants, on platforms like eg mac uses different
    rendering sizes.
*/
enum wxWindowVariant
{
    wxWINDOW_VARIANT_NORMAL,  //!< Normal size
    wxWINDOW_VARIANT_SMALL,   //!< Smaller size (about 25 % smaller than normal)
    wxWINDOW_VARIANT_MINI,    //!< Mini size (about 33 % smaller than normal)
    wxWINDOW_VARIANT_LARGE,   //!< Large size (about 25 % larger than normal)
    wxWINDOW_VARIANT_MAX
};


/**
    @class wxWindow

    wxWindow is the base class for all windows and represents any visible object
    on screen. All controls, top level windows and so on are windows. Sizers and
    device contexts are not, however, as they don't appear on screen themselves.

    Please note that all children of the window will be deleted automatically by
    the destructor before the window itself is deleted which means that you don't
    have to worry about deleting them manually. Please see the @ref
    overview_windowdeletion "window deletion overview" for more information.

    Also note that in this, and many others, wxWidgets classes some
    @c GetXXX() methods may be overloaded (as, for example,
    wxWindow::GetSize or wxWindow::GetClientSize). In this case, the overloads
    are non-virtual because having multiple virtual functions with the same name
    results in a virtual function name hiding at the derived class level (in
    English, this means that the derived class has to override all overloaded
    variants if it overrides any of them). To allow overriding them in the derived
    class, wxWidgets uses a unique protected virtual @c DoGetXXX() method
    and all @c GetXXX() ones are forwarded to it, so overriding the former
    changes the behaviour of the latter.

    @beginStyleTable
    @style{wxBORDER_DEFAULT}
           The window class will decide the kind of border to show, if any.
    @style{wxBORDER_SIMPLE}
           Displays a thin border around the window. wxSIMPLE_BORDER is the
           old name for this style.
    @style{wxBORDER_SUNKEN}
           Displays a sunken border. wxSUNKEN_BORDER is the old name for this
           style.
    @style{wxBORDER_RAISED}
           Displays a raised border. wxRAISED_BORDER is the old name for this
           style.
    @style{wxBORDER_STATIC}
           Displays a border suitable for a static control.  wxSTATIC_BORDER
           is the old name for this style. Windows only.
    @style{wxBORDER_THEME}
           Displays a native border suitable for a control, on the current
           platform. On Windows, this will be a themed border; on
           most other platforms a sunken border will be used. For more
           information for themed borders on Windows, please see Themed
           borders on Windows.
    @style{wxBORDER_NONE}
           Displays no border, overriding the default border style for the
           window. wxNO_BORDER is the old name for this style.
    @style{wxBORDER_DOUBLE}
           This style is obsolete and should not be used.
    @style{wxTRANSPARENT_WINDOW}
           The window is transparent, that is, it will not receive paint
           events. Windows only.
    @style{wxTAB_TRAVERSAL}
           This style is used by wxWidgets for the windows supporting TAB
           navigation among their children, such as wxDialog and wxPanel. It
           should almost never be used in the application code.
    @style{wxWANTS_CHARS}
           Use this to indicate that the window wants to get all char/key
           events for all keys - even for keys like TAB or ENTER which are
           usually used for dialog navigation and which wouldn't be generated
           without this style.  If you need to use this style in order to get
           the arrows or etc., but would still like to have normal keyboard
           navigation take place, you should call Navigate in response to the
           key events for Tab and Shift-Tab.
    @style{wxNO_FULL_REPAINT_ON_RESIZE}
           On Windows, this style used to disable repainting the window
           completely when its size is changed. Since this behaviour is now
           the default, the style is now obsolete and no longer has an effect.
    @style{wxVSCROLL}
           Use this style to enable a vertical scrollbar. Notice that this
           style cannot be used with native controls which don't support
           scrollbars nor with top-level windows in most ports.
    @style{wxHSCROLL}
           Use this style to enable a horizontal scrollbar. The same
           limitations as for wxVSCROLL apply to this style.
    @style{wxALWAYS_SHOW_SB}
           If a window has scrollbars, disable them instead of hiding them
           when they are not needed (i.e. when the size of the window is big
           enough to not require the scrollbars to navigate it). This style is
           currently implemented for wxMSW, wxGTK and wxUniversal and does
           nothing on the other platforms.
    @style{wxCLIP_CHILDREN}
           Use this style to eliminate flicker caused by the background being
           repainted, then children being painted over them. Windows only.
    @style{wxFULL_REPAINT_ON_RESIZE}
           Use this style to force a complete redraw of the window whenever it
           is resized instead of redrawing just the part of the window
           affected by resizing. Note that this was the behaviour by default
           before 2.5.1 release and that if you experience redraw problems
           with code which previously used to work you may want to try this.
           Currently this style applies on GTK+ 2 and Windows only, and full
           repainting is always done on other platforms.
    @endStyleTable

    @beginExtraStyleTable
    @style{wxWS_EX_BLOCK_EVENTS}
           wxCommandEvents and the objects of the derived classes are
           forwarded to the parent window and so on recursively by default.
           Using this flag for the given window allows blocking this
           propagation at this window, i.e. prevent the events from being
           propagated further upwards. Dialogs have this flag on by default
           for the reasons explained in the @ref overview_events.
    @style{wxWS_EX_TRANSIENT}
           Don't use this window as an implicit parent for the other windows:
           this must be used with transient windows as otherwise there is the
           risk of creating a dialog/frame with this window as a parent, which
           would lead to a crash if the parent were destroyed before the child.
    @style{wxWS_EX_CONTEXTHELP}
           Under Windows, puts a query button on the caption. When pressed,
           Windows will go into a context-sensitive help mode and wxWidgets
           will send a @c wxEVT_HELP event if the user clicked on an application window.
           This style cannot be used (because of the underlying native behaviour)
           together with @c wxMAXIMIZE_BOX or @c wxMINIMIZE_BOX, so these two styles
           are automatically turned off if this one is used.
    @style{wxWS_EX_PROCESS_IDLE}
           This window should always process idle events, even if the mode set
           by wxIdleEvent::SetMode is @c wxIDLE_PROCESS_SPECIFIED.
    @style{wxWS_EX_PROCESS_UI_UPDATES}
           This window should always process UI update events, even if the
           mode set by wxUpdateUIEvent::SetMode is @c wxUPDATE_UI_PROCESS_SPECIFIED.
    @endExtraStyleTable

    @beginEventEmissionTable
    @event{EVT_ACTIVATE(id, func)}
        Process a @c wxEVT_ACTIVATE event. See wxActivateEvent.
    @event{EVT_CHILD_FOCUS(func)}
        Process a @c wxEVT_CHILD_FOCUS event. See wxChildFocusEvent.
    @event{EVT_CONTEXT_MENU(func)}
        A right click (or other context menu command depending on platform) has been detected.
        See wxContextMenuEvent.
    @event{EVT_HELP(id, func)}
        Process a @c wxEVT_HELP event. See wxHelpEvent.
    @event{EVT_HELP_RANGE(id1, id2, func)}
        Process a @c wxEVT_HELP event for a range of ids. See wxHelpEvent.
    @event{EVT_DROP_FILES(func)}
        Process a @c wxEVT_DROP_FILES event. See wxDropFilesEvent.
    @event{EVT_ERASE_BACKGROUND(func)}
        Process a @c wxEVT_ERASE_BACKGROUND event. See wxEraseEvent.
    @event{EVT_SET_FOCUS(func)}
        Process a @c wxEVT_SET_FOCUS event. See wxFocusEvent.
    @event{EVT_KILL_FOCUS(func)}
        Process a @c wxEVT_KILL_FOCUS event. See wxFocusEvent.
    @event{EVT_IDLE(func)}
        Process a @c wxEVT_IDLE event. See wxIdleEvent.
    @event{EVT_JOY_*(func)}
        Processes joystick events. See wxJoystickEvent.
    @event{EVT_KEY_DOWN(func)}
        Process a @c wxEVT_KEY_DOWN event (any key has been pressed).
        See wxKeyEvent.
    @event{EVT_KEY_UP(func)}
        Process a @c wxEVT_KEY_UP event (any key has been released).
        See wxKeyEvent.
    @event{EVT_CHAR(func)}
        Process a @c wxEVT_CHAR event.
        See wxKeyEvent.
    @event{EVT_CHAR_HOOK(func)}
        Process a @c wxEVT_CHAR_HOOK event.
        See wxKeyEvent.
    @event{EVT_MOUSE_CAPTURE_LOST(func)}
        Process a @c wxEVT_MOUSE_CAPTURE_LOST event. See wxMouseCaptureLostEvent.
    @event{EVT_MOUSE_CAPTURE_CHANGED(func)}
        Process a @c wxEVT_MOUSE_CAPTURE_CHANGED event. See wxMouseCaptureChangedEvent.
    @event{EVT_MOUSE_*(func)}
        See wxMouseEvent.
    @event{EVT_PAINT(func)}
        Process a @c wxEVT_PAINT event. See wxPaintEvent.
    @event{EVT_POWER_*(func)}
        The system power state changed. See wxPowerEvent.
    @event{EVT_SCROLLWIN_*(func)}
        Process scroll events. See wxScrollWinEvent.
    @event{EVT_SET_CURSOR(func)}
        Process a @c wxEVT_SET_CURSOR event. See wxSetCursorEvent.
    @event{EVT_SIZE(func)}
        Process a @c wxEVT_SIZE event. See wxSizeEvent.
    @event{EVT_SYS_COLOUR_CHANGED(func)}
        Process a @c wxEVT_SYS_COLOUR_CHANGED event. See wxSysColourChangedEvent.
    @endEventTable

    @library{wxcore}
    @category{miscwnd}

    @see @ref overview_events, @ref overview_windowsizing
*/
class wxWindow : public wxEvtHandler
{
public:
    /**
       Default constructor
    */
    wxWindow();

    /**
        Constructs a window, which can be a child of a frame, dialog or any other
        non-control window.

        @param parent
            Pointer to a parent window.
        @param id
            Window identifier. If wxID_ANY, will automatically create an identifier.
            See @ref overview_windowids for more information about IDs.
        @param pos
            Window position. wxDefaultPosition indicates that wxWidgets
            should generate a default position for the window.
            If using the wxWindow class directly, supply an actual position.
        @param size
            Window size. wxDefaultSize indicates that wxWidgets should generate
            a default size for the window. If no suitable size can  be found, the
            window will be sized to 20x20 pixels so that the window is visible but
            obviously not correctly sized.
        @param style
            Window style. For generic window styles, please see wxWindow.
        @param name
            Window name.
    */
    wxWindow(wxWindow* parent, wxWindowID id,
             const wxPoint& pos = wxDefaultPosition,
             const wxSize& size = wxDefaultSize,
             long style = 0,
             const wxString& name = wxPanelNameStr);

    /**
        Destructor.

        Deletes all sub-windows, then deletes itself. Instead of using
        the @b delete operator explicitly, you should normally  use Destroy()
        so that wxWidgets can delete a window only when it is safe to do so, in idle time.

        @see @ref overview_windowdeletion "Window Deletion Overview",
             Destroy(), wxCloseEvent
    */
    virtual ~wxWindow();


    /**
        Construct the actual window object after creating the C++ object.

        The non-default constructor of wxWindow class does two things: it
        initializes the C++ object and it also creates the window object in the
        underlying graphical toolkit. The Create() method can be used to
        perform the second part later, while the default constructor can be
        used to perform the first part only.

        Please note that the underlying window must be created exactly once,
        i.e. if you use the default constructor, which doesn't do this, you @em
        must call Create() before using the window and if you use the
        non-default constructor, you can @em not call Create(), as the
        underlying window is already created.

        Note that it is possible and, in fact, useful, to call some methods on
        the object between creating the C++ object itself and calling Create()
        on it, e.g. a common pattern to avoid showing the contents of a window
        before it is fully initialized is:
        @code
            wxPanel* panel = new wxPanel(); // Note: default constructor used.
            panel->Hide(); // Can be called before actually creating it.
            panel->Create(parent, wxID_ANY, ...); // Won't be shown yet.
            ... create all the panel children ...
            panel->Show(); // Now everything will be shown at once.
        @endcode

        Also note that it is possible to create an object of a derived type and
        then call Create() on it:
        @code
            // Suppose we have this function (which would typically be in a
            // different translation unit (file) from the rest of the code).
            wxWindow* MyCreateWindowObjectFunction() {
                return new MyCustomClassDerivingFromWindow();
            }

            // Then we can create a window of MyCustomClassDerivingFromWindow
            // class without really knowing about this type, as we would have
            // to do if we wanted to use the non-default constructor, like this:

            // First create the C++ object using the factory function.
            wxWindow* window = MyCreateWindowObjectFunction();

            // And now create the underlying window.
            //
            // This calls the base wxWindow::Create() as it is not virtual, so
            // the derived class can't customize this part.
            window->Create(parent, wxID_ANY, ...);
        @endcode
        This is notably used by @ref overview_xrc.

        The parameters of this method have exactly the same meaning as the
        non-default constructor parameters, please refer to them for their
        description.

        @return @true if window creation succeeded or @false if it failed
     */
    bool Create(wxWindow *parent,
                wxWindowID id,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = 0,
                const wxString& name = wxPanelNameStr);

    /**
        @name Focus functions

        See also the static function FindFocus().
    */
    //@{

    /**
        This method may be overridden in the derived classes to return @false to
        indicate that this control doesn't accept input at all (i.e.\ behaves like
        e.g.\ wxStaticText) and so doesn't need focus.

        @see AcceptsFocusFromKeyboard()
    */
    virtual bool AcceptsFocus() const;

    /**
        This method may be overridden in the derived classes to return @false to
        indicate that while this control can, in principle, have focus if the user
        clicks it with the mouse, it shouldn't be included in the TAB traversal chain
        when using the keyboard.
    */
    virtual bool AcceptsFocusFromKeyboard() const;

     /**
        Overridden to indicate whether this window or one of its children accepts
        focus. Usually it's the same as AcceptsFocus() but is overridden for
        container windows.
     */
    virtual bool AcceptsFocusRecursively() const;

    /**
        Disable giving focus to this window using the keyboard navigation keys.

        Pressing @c TAB key will skip this window if this function was called
        on it, but it will still be possible to focus it by clicking on it with
        a pointing device.

        @since 3.1.4
     */
    void DisableFocusFromKeyboard();

    /**
     Can this window itself have focus?
    */
    bool IsFocusable() const;

    /**
       Can this window have focus right now?

       If this method returns true, it means that calling SetFocus() will
       put focus either to this window or one of its children, if you need
       to know whether this window accepts focus itself, use IsFocusable()
    */
    bool CanAcceptFocus() const;

    /**
       Can this window be assigned focus from keyboard right now?
    */
    bool CanAcceptFocusFromKeyboard() const;


    /**
        Returns @true if the window (or in case of composite controls, its main
        child window) has focus.

        @since 2.9.0

        @see FindFocus()
    */
    virtual bool HasFocus() const;

    /**
        This method is only implemented by ports which have support for
        native TAB traversal (such as GTK+ 2.0).

        It is called by wxWidgets' container control code to give the native
        system a hint when doing TAB traversal. A call to this does not disable
        or change the effect of programmatically calling SetFocus().

        @see wxFocusEvent, wxPanel::SetFocus, wxPanel::SetFocusIgnoringChildren
    */
    virtual void SetCanFocus(bool canFocus);

    /**
        This sets the window to receive keyboard input.

        @see HasFocus(), wxFocusEvent, wxPanel::SetFocus,
             wxPanel::SetFocusIgnoringChildren
    */
    virtual void SetFocus();

    /**
        This function is called by wxWidgets keyboard navigation code when the user
        gives the focus to this window from keyboard (e.g.\ using @c TAB key).

        By default this method simply calls SetFocus() but
        can be overridden to do something in addition to this in the derived classes.
    */
    virtual void SetFocusFromKbd();

    //@}


    /**
        @name Child management functions
    */
    //@{

    /**
        Adds a child window. This is called automatically by window creation
        functions so should not be required by the application programmer.
        Notice that this function is mostly internal to wxWidgets and shouldn't be
        called by the user code.

        @param child
            Child window to add.
    */
    virtual void AddChild(wxWindow* child);

    /**
        Destroys all children of a window. Called automatically by the destructor.
    */
    bool DestroyChildren();

    /**
        Find a child of this window, by @a id.

        May return @a this if it matches itself.

        Notice that only real children, not top level windows using this window
        as parent, are searched by this function.
    */
    wxWindow* FindWindow(long id) const;

    /**
        Find a child of this window, by name.

        May return @a this if it matches itself.

        Notice that only real children, not top level windows using this window
        as parent, are searched by this function.
    */
    wxWindow* FindWindow(const wxString& name) const;

    /**
        Returns a reference to the list of the window's children. @c wxWindowList
        is a type-safe wxList-like class whose elements are of type @c wxWindow*.
    */
    wxWindowList& GetChildren();

    /**
        Returns a const reference to the list of the window's children.

        @copydetails GetChildren()
     */
    const wxWindowList& GetChildren() const;

    /**
        Removes a child window.

        This is called automatically by window deletion functions so should not
        be required by the application programmer.
        Notice that this function is mostly internal to wxWidgets and shouldn't be
        called by the user code.

        @param child
            Child window to remove.
    */
    virtual void RemoveChild(wxWindow* child);

    //@}


    /**
        @name Sibling and parent management functions
    */
    //@{

    /**
        Returns the grandparent of a window, or @NULL if there isn't one.
    */
    wxWindow* GetGrandParent() const;

    /**
        Returns the next window after this one among the parent's children or @NULL
        if this window is the last child.

        @since 2.8.8

        @see GetPrevSibling()
    */
    wxWindow* GetNextSibling() const;

    /**
        Returns the parent of the window, or @NULL if there is no parent.
    */
    wxWindow* GetParent() const;

    /**
        Returns the previous window before this one among the parent's children or @c
        @NULL if this window is the first child.

        @since 2.8.8

        @see GetNextSibling()
    */
    wxWindow* GetPrevSibling() const;

    /**
        Check if the specified window is a descendant of this one.

        Returns @true if the window is a descendant (i.e. a child or
        grand-child or grand-grand-child or ...) of this one.

        Notice that a window can never be a descendant of another one if they
        are in different top level windows, i.e. a child of a wxDialog is not
        considered to be a descendant of dialogs parent wxFrame.

        @param win Any window, possible @NULL (@false is always returned then).

        @since 2.9.4
     */
    bool IsDescendant(wxWindow* win) const;

    /**
        Reparents the window, i.e.\ the window will be removed from its
        current parent window (e.g. a non-standard toolbar in a wxFrame)
        and then re-inserted into another.

        Notice that currently you need to explicitly call
        wxNotebook::RemovePage() before reparenting a notebook page.

        @param newParent
            New parent.
    */
    virtual bool Reparent(wxWindow* newParent);

    //@}


    /**
        @name Scrolling and scrollbars functions

        Note that these methods don't work with native controls which don't use
        wxWidgets scrolling framework (i.e. don't derive from wxScrolledWindow).
    */
    //@{

    /**
        Call this function to force one or both scrollbars to be always shown, even if
        the window is big enough to show its entire contents without scrolling.

        @since 2.9.0

        @param hflag
            Whether the horizontal scroll bar should always be visible.
        @param vflag
            Whether the vertical scroll bar should always be visible.

        @remarks This function is currently not implemented.
    */
    virtual void AlwaysShowScrollbars(bool hflag = true, bool vflag = true);

    /**
        Returns the built-in scrollbar position.

        @see SetScrollbar()
    */
    virtual int GetScrollPos(int orientation) const;

    /**
        Returns the built-in scrollbar range.

        @see SetScrollbar()
    */
    virtual int GetScrollRange(int orientation) const;

    /**
        Returns the built-in scrollbar thumb size.

        @see SetScrollbar()
    */
    virtual int GetScrollThumb(int orientation) const;

    /**
        Returns @true if this window can have a scroll bar in this orientation.

        @param orient
            Orientation to check, either wxHORIZONTAL or wxVERTICAL.

        @since 2.9.1
    */
    bool CanScroll(int orient) const;

    /**
        Returns @true if this window currently has a scroll bar for this
        orientation.

        This method may return @false even when CanScroll() for the same
        orientation returns @true, but if CanScroll() returns @false, i.e.
        scrolling in this direction is not enabled at all, HasScrollbar()
        always returns @false as well.

        @param orient
            Orientation to check, either wxHORIZONTAL or wxVERTICAL.
    */
    bool HasScrollbar(int orient) const;

    /**
        Return whether a scrollbar is always shown.

        @param orient
            Orientation to check, either wxHORIZONTAL or wxVERTICAL.

        @see AlwaysShowScrollbars()
    */
    virtual bool IsScrollbarAlwaysShown(int orient) const;

    /**
        Scrolls the window by the given number of lines down (if @a lines is
        positive) or up.

        @return Returns @true if the window was scrolled, @false if it was already
                on top/bottom and nothing was done.

        @remarks This function is currently only implemented under MSW and
                 wxTextCtrl under wxGTK (it also works for wxScrolled classes
                 under all platforms).

        @see ScrollPages()
    */
    virtual bool ScrollLines(int lines);

    /**
        Scrolls the window by the given number of pages down (if @a pages is
        positive) or up.

        @return Returns @true if the window was scrolled, @false if it was already
                on top/bottom and nothing was done.

        @remarks This function is currently only implemented under MSW and wxGTK.

        @see ScrollLines()
    */
    virtual bool ScrollPages(int pages);

    /**
        Physically scrolls the pixels in the window and move child windows accordingly.

        @param dx
            Amount to scroll horizontally.
        @param dy
            Amount to scroll vertically.
        @param rect
            Rectangle to scroll, if it is @NULL, the whole window is
            scrolled (this is always the case under wxGTK which doesn't support this
            parameter)

        @remarks Note that you can often use wxScrolled instead of using this
                 function directly.
    */
    virtual void ScrollWindow(int dx, int dy,
                              const wxRect* rect = NULL);

    /**
        Same as #ScrollLines (-1).
    */
    bool LineUp();

    /**
        Same as #ScrollLines (1).
    */
    bool LineDown();

    /**
        Same as #ScrollPages (-1).
    */
    bool PageUp();

    /**
        Same as #ScrollPages (1).
    */
    bool PageDown();

    /**
        Sets the position of one of the built-in scrollbars.

        @param orientation
            Determines the scrollbar whose position is to be set.
            May be wxHORIZONTAL or wxVERTICAL.
        @param pos
            Position in scroll units.
        @param refresh
            @true to redraw the scrollbar, @false otherwise.

        @remarks This function does not directly affect the contents of the
                 window: it is up to the application to take note of
                 scrollbar attributes and redraw contents accordingly.

        @see SetScrollbar(), GetScrollPos(), GetScrollThumb(), wxScrollBar,
             wxScrolled
    */
    virtual void SetScrollPos(int orientation, int pos,
                              bool refresh = true);

    /**
        Sets the scrollbar properties of a built-in scrollbar.

        @param orientation
            Determines the scrollbar whose page size is to be set.
            May be wxHORIZONTAL or wxVERTICAL.
        @param position
            The position of the scrollbar in scroll units.
        @param thumbSize
            The size of the thumb, or visible portion of the scrollbar, in scroll units.
        @param range
            The maximum position of the scrollbar. Value of -1 can be used to
            ask for the scrollbar to be shown but in the disabled state: this
            can be used to avoid removing the scrollbar even when it is not
            needed (currently this is only implemented in wxMSW port).
        @param refresh
            @true to redraw the scrollbar, @false otherwise.

        @remarks
            Let's say you wish to display 50 lines of text, using the same font.
            The window is sized so that you can only see 16 lines at a time.
            You would use:
            @code
            SetScrollbar(wxVERTICAL, 0, 16, 50);
            @endcode
            Note that with the window at this size, the thumb position can never
            go above 50 minus 16, or 34. You can determine how many lines are
            currently visible by dividing the current view size by the character
            height in pixels.
            When defining your own scrollbar behaviour, you will always need
            to recalculate the scrollbar settings when the window size changes.
            You could therefore put your scrollbar calculations and SetScrollbar
            call into a function named AdjustScrollbars, which can be called
            initially and also from your wxSizeEvent handler function.

        @see @ref overview_scrolling, wxScrollBar, wxScrolled, wxScrollWinEvent
    */
    virtual void SetScrollbar(int orientation, int position,
                              int thumbSize, int range,
                              bool refresh = true);
    //@}


    /**
        @name Sizing functions

        See also the protected functions DoGetBestSize() and
        DoGetBestClientSize().
    */
    //@{

    /**
        Helper for ensuring EndRepositioningChildren() is called correctly.

        This class wraps the calls to BeginRepositioningChildren() and
        EndRepositioningChildren() by performing the former in its constructor
        and the latter in its destructor if, and only if, the first call
        returned @true. This is the simplest way to call these methods and if
        this class is created as a local variable, it also ensures that
        EndRepositioningChildren() is correctly called (or not) on scope exit,
        so its use instead of calling these methods manually is highly
        recommended.

        @since 2.9.5
     */
    class ChildrenRepositioningGuard
    {
    public:
        /**
            Constructor calls wxWindow::BeginRepositioningChildren().

            @param win The window to call BeginRepositioningChildren() on. If
                it is @NULL, nothing is done.
         */
        explicit ChildrenRepositioningGuard(wxWindow* win);

        /**
            Destructor calls wxWindow::EndRepositioningChildren() if necessary.

            EndRepositioningChildren() is called only if a valid window was
            passed to the constructor and if BeginRepositioningChildren()
            returned @true.
         */
        ~ChildrenRepositioningGuard();
    };

    /**
        Prepare for changing positions of multiple child windows.

        This method should be called before changing positions of multiple
        child windows to reduce flicker and, in MSW case, even avoid display
        corruption in some cases. It is used internally by wxWidgets and called
        automatically when the window size changes but it can also be useful to
        call it from outside of the library if a repositioning involving
        multiple children is done without changing the window size.

        If this method returns @true, then EndRepositioningChildren() must be
        called after setting all children positions. Use
        ChildrenRepositioningGuard class to ensure that this requirement is
        satisfied.

        @since 2.9.5
     */
    bool BeginRepositioningChildren();

    /**
        Fix child window positions after setting all of them at once.

        This method must be called if and only if the previous call to
        BeginRepositioningChildren() returned @true.

        @since 2.9.5
     */
    void EndRepositioningChildren();

    /**
        Sets the cached best size value.

        @see GetBestSize()
    */
    void CacheBestSize(const wxSize& size) const;

    /**
        Converts client area size @a size to corresponding window size.

        In other words, the returned value is what would GetSize() return if this
        window had client area of given size.  Components with wxDefaultCoord
        value are left unchanged.  Note that the conversion is not always
        exact, it assumes that non-client area doesn't change and so doesn't
        take into account things like menu bar (un)wrapping or (dis)appearance
        of the scrollbars.

        @since 2.8.8

        @see WindowToClientSize()
    */
    virtual wxSize ClientToWindowSize(const wxSize& size) const;

    /**
        Converts window size @a size to corresponding client area size
        In other words, the returned value is what would GetClientSize() return if
        this window had given window size. Components with wxDefaultCoord value
        are left unchanged.

        Note that the conversion is not always exact, it assumes that
        non-client area doesn't change and so doesn't take into account things
        like menu bar (un)wrapping or (dis)appearance of the scrollbars.

        @since 2.8.8

        @see ClientToWindowSize()
    */
    virtual wxSize WindowToClientSize(const wxSize& size) const;

    /**
        Sizes the window to fit its best size.

        Using this function is equivalent to setting window size to the return
        value of GetBestSize().

        Note that, unlike SetSizerAndFit(), this function only changes the
        current window size and doesn't change its minimal size.

        @see @ref overview_windowsizing
    */
    virtual void Fit();

    /**
        Similar to Fit(), but sizes the interior (virtual) size of a window.

        Mainly useful with scrolled windows to reset scrollbars after sizing
        changes that do not trigger a size event, and/or scrolled windows without
        an interior sizer.  This function similarly won't do anything if there are
        no subwindows.
    */
    virtual void FitInside();

    /**
        Convert DPI-independent pixel values to the value in pixels appropriate
        for the current toolkit.

        A DPI-independent pixel is just a pixel at the standard 96 DPI
        resolution. To keep the same physical size at higher resolution, the
        physical pixel value must be scaled by GetDPIScaleFactor() but this
        scaling may be already done by the underlying toolkit (GTK+, Cocoa,
        ...) automatically. This method performs the conversion only if it is
        not already done by the lower level toolkit and so by using it with
        pixel values you can guarantee that the physical size of the
        corresponding elements will remain the same in all resolutions under
        all platforms. For example, instead of creating a bitmap of the hard
        coded size of 32 pixels you should use
        @code
            wxBitmap bmp(FromDIP(32, 32));
        @endcode
        to avoid using tiny bitmaps on high DPI screens.

        Notice that this function is only needed when using hard coded pixel
        values. It is not necessary if the sizes are already based on the
        DPI-independent units such as dialog units or if you are relying on the
        controls automatic best size determination and using sizers to lay out
        them.

        Also note that if either component of @a sz has the special value of
        -1, it is returned unchanged independently of the current DPI, to
        preserve the special value of -1 in wxWidgets API (it is often used to
        mean "unspecified").

        @since 3.1.0
     */
    wxSize FromDIP(const wxSize& sz) const;

    /// @overload
    wxPoint FromDIP(const wxPoint& pt) const;

    /**
        Convert DPI-independent distance in pixels to the value in pixels
        appropriate for the current toolkit.

        This is the same as FromDIP(const wxSize& sz) overload, but assumes
        that the resolution is the same in horizontal and vertical directions.

        If @a d has the special value of -1, it is returned unchanged
        independently of the current DPI.

        @since 3.1.0
     */
    int FromDIP(int d) const;

    /**
        Non window-specific DPI-independent pixels conversion functions.

        The display resolution depends on the window in general as different
        windows can appear on different monitors using different resolutions,
        however sometimes no window is available for converting the resolution
        independent pixels to the physical values and in this case these static
        overloads can be used with @NULL value for @a w argument.

        Using these methods is discouraged as passing @NULL will prevent your
        application from correctly supporting monitors with different
        resolutions even in the future wxWidgets versions which will add
        support for them, and passing non-@NULL window is just a less
        convenient way of calling the non-static FromDIP() method.

        @since 3.1.0
     */
    static wxSize FromDIP(const wxSize& sz, const wxWindow* w);

    /// @overload
    static wxPoint FromDIP(const wxPoint& pt, const wxWindow* w);

    /// @overload
    static int FromDIP(int d, const wxWindow* w);


    /**
    Convert pixel values of the current toolkit to DPI-independent pixel values.

    A DPI-independent pixel is just a pixel at the standard 96 DPI
    resolution. To keep the same physical size at higher resolution, the
    physical pixel value must be scaled by GetDPIScaleFactor() but this
    scaling may be already done by the underlying toolkit (GTK+, Cocoa,
    ...) automatically. This method performs the conversion only if it is
    not already done by the lower level toolkit, For example, you may
    want to use this to store window sizes and positions so that they
    can be re-used regardless of the display DPI:
    @code
    wxPoint pt(ToDIP(GetPosition()));
    wxSize size(ToDIP(GetSize()));
    @endcode

    Also note that if either component of @a sz has the special value of
    -1, it is returned unchanged independently of the current DPI, to
    preserve the special value of -1 in wxWidgets API (it is often used to
    mean "unspecified").

    @since 3.1.0
    */
    wxSize ToDIP(const wxSize& sz) const;

    /// @overload
    wxPoint ToDIP(const wxPoint& pt) const;

    /**
    Convert pixel values of the current toolkit to DPI-independent pixel values.

    This is the same as ToDIP(const wxSize& sz) overload, but assumes
    that the resolution is the same in horizontal and vertical directions.

    If @a d has the special value of -1, it is returned unchanged
    independently of the current DPI.

    @since 3.1.0
    */
    int ToDIP(int d) const;

    /**
    Non window-specific pixel to DPI-independent pixels conversion functions.

    The display resolution depends on the window in general as different
    windows can appear on different monitors using different resolutions,
    however sometimes no window is available for converting the resolution
    independent pixels to the physical values and in this case these static
    overloads can be used with @NULL value for @a w argument.

    Using these methods is discouraged as passing @NULL will prevent your
    application from correctly supporting monitors with different
    resolutions even in the future wxWidgets versions which will add
    support for them, and passing non-@NULL window is just a less
    convenient way of calling the non-static ToDIP() method.

    @since 3.1.0
    */
    static wxSize ToDIP(const wxSize& sz, const wxWindow* w);

    /// @overload
    static wxPoint ToDIP(const wxPoint& pt, const wxWindow* w);

    /// @overload
    static int ToDIP(int d, const wxWindow* w);

    /**
        This functions returns the best acceptable minimal size for the window.

        For example, for a static control, it will be the minimal size such that the
        control label is not truncated. For windows containing subwindows (typically
        wxPanel), the size returned by this function will be the same as the size
        the window would have had after calling Fit().

        Override virtual DoGetBestSize() or, better, because it's usually more
        convenient, DoGetBestClientSize() when writing your own custom window
        class to change the value returned by this public non-virtual method.

        Notice that the best size respects the minimal and maximal size
        explicitly set for the window, if any. So even if some window believes
        that it needs 200 pixels horizontally, calling SetMaxSize() with a
        width of 100 would ensure that GetBestSize() returns the width of at
        most 100 pixels.

        @see CacheBestSize(), @ref overview_windowsizing
    */
    wxSize GetBestSize() const;

    /**
        Returns the best height needed by this window if it had the given width.

        @see DoGetBestClientHeight()

        @since 2.9.4
     */
    int GetBestHeight(int width) const;

    /**
        Returns the best width needed by this window if it had the given height.

        @see DoGetBestClientWidth()

        @since 2.9.4
     */
    int GetBestWidth(int height) const;

    /**
        Returns the size of the window 'client area' in pixels.

        The client area is the area which may be drawn on by the programmer,
        excluding title bar, border, scrollbars, etc.
        Note that if this window is a top-level one and it is currently minimized, the
        return size is empty (both width and height are 0).

        @beginWxPerlOnly
        In wxPerl this method takes no parameters and returns
        a 2-element list (width, height).
        @endWxPerlOnly

        @see GetSize(), GetVirtualSize()
    */
    void GetClientSize(int* width, int* height) const;

    /**
        @overload
    */
    wxSize GetClientSize() const;

    /**
        Merges the window's best size into the min size and returns the result.
        This is the value used by sizers to determine the appropriate
        amount of space to allocate for the widget.

        This is the method called by a wxSizer when it queries the size
        of a window or control.

        @see GetBestSize(), SetInitialSize(), @ref overview_windowsizing
    */
    virtual wxSize GetEffectiveMinSize() const;

    /**
        Returns the maximum size of window's client area.

        This is an indication to the sizer layout mechanism that this is the maximum
        possible size as well as the upper bound on window's size settable using
        SetClientSize().

        @see GetMaxSize(), @ref overview_windowsizing
    */
    virtual wxSize GetMaxClientSize() const;

    /**
        Returns the maximum size of the window.

        This is an indication to the sizer layout mechanism that this is the maximum
        possible size as well as the upper bound on window's size settable using SetSize().

        @see GetMaxClientSize(), @ref overview_windowsizing
    */
    virtual wxSize GetMaxSize() const;

    /**
        Returns the minimum size of window's client area, an indication to the sizer
        layout mechanism that this is the minimum required size of its client area.

        It normally just returns the value set by SetMinClientSize(), but it can be
        overridden to do the calculation on demand.

        @see GetMinSize(), @ref overview_windowsizing
    */
    virtual wxSize GetMinClientSize() const;

    /**
        Returns the minimum size of the window, an indication to the sizer layout
        mechanism that this is the minimum required size.

        This method normally just returns the value set by SetMinSize(), but it
        can be overridden to do the calculation on demand.

        @see GetMinClientSize(), @ref overview_windowsizing
    */
    virtual wxSize GetMinSize() const;

    /**
        Returns the horizontal component of window minimal size.

        The returned value is wxDefaultCoord if the minimal width was not set.

        @see GetMinSize()
     */
    int GetMinWidth() const;

    /**
        Returns the vertical component of window minimal size.

        The returned value is wxDefaultCoord if the minimal height was not set.

        @see GetMinSize()
     */
    int GetMinHeight() const;

    /**
        Returns the horizontal component of window maximal size.

        The returned value is wxDefaultCoord if the maximal width was not set.

        @see GetMaxSize()
     */
    int GetMaxWidth() const;

    /**
        Returns the vertical component of window maximal size.

        The returned value is wxDefaultCoord if the maximal width was not set.

        @see GetMaxSize()
     */
    int GetMaxHeight() const;

    /**
        Returns the size of the entire window in pixels, including title bar, border,
        scrollbars, etc.

        Note that if this window is a top-level one and it is currently minimized, the
        returned size is the restored window size, not the size of the window icon.

        @param width
            Receives the window width.
        @param height
            Receives the window height.

        @beginWxPerlOnly
        In wxPerl this method is implemented as GetSizeWH() returning
        a 2-element list (width, height).
        @endWxPerlOnly

        @see GetClientSize(), GetVirtualSize(), @ref overview_windowsizing
    */
    void GetSize(int* width, int* height) const;

    /**
        See the GetSize(int*,int*) overload for more info.
    */
    wxSize GetSize() const;

    /**
        This gets the virtual size of the window in pixels.
        By default it returns the client size of the window, but after a call to
        SetVirtualSize() it will return the size set with that method.

        @see @ref overview_windowsizing
    */
    wxSize GetVirtualSize() const;

    /**
        Like the other GetVirtualSize() overload but uses pointers instead.

        @param width
            Receives the window virtual width.
        @param height
            Receives the window virtual height.
    */
    void GetVirtualSize(int* width, int* height) const;

    /**
       Return the largest of ClientSize and BestSize (as determined
       by a sizer, interior children, or other means)
    */
    virtual wxSize GetBestVirtualSize() const;

    /**
       Returns the factor mapping logical pixels of this window to physical
       pixels.

       This function can be used to portably determine the number of physical
       pixels in a window of the given size, by multiplying the window size by
       the value returned from it. I.e. it returns the factor converting window
       coordinates to "content view" coordinates, where the view can be just a
       simple window displaying a wxBitmap or wxGLCanvas or any other kind of
       window rendering arbitrary "content" on screen.

       For the platforms not doing any pixel mapping, i.e. where logical and
       physical pixels are one and the same, this function always returns 1.0
       and so using it is, in principle, unnecessary and could be avoided by
       using preprocessor check for @c wxHAVE_DPI_INDEPENDENT_PIXELS @e not
       being defined, however using this function unconditionally under all
       platforms is usually simpler and so preferable.

       @note Current behaviour of this function is compatible with wxWidgets
           3.0, but different from its behaviour in versions 3.1.0 to 3.1.3,
           where it returned the same value as GetDPIScaleFactor(). Please use
           the other function if you need to use a scaling factor greater than
           1.0 even for the platforms without @c wxHAVE_DPI_INDEPENDENT_PIXELS,
           such as wxMSW.

       @since 2.9.5
    */
    double GetContentScaleFactor() const;

    /**
       Returns the ratio of the DPI used by this window to the standard DPI.

       The returned value is 1 for standard DPI screens or 2 for "200%
       scaling" and, unlike for GetContentScaleFactor(), is the same under all
       platforms.

       This factor should be used to increase the size of icons and similar
       windows whose best size is not based on text metrics when using DPI
       scaling.

       E.g. the program may load a 32px bitmap if the content scale factor is
       1.0 or 64px version of the same bitmap if it is 2.0 or bigger.

       Notice that this method should @e not be used for window sizes expressed
       in pixels, as they are already scaled by this factor by the underlying
       toolkit under some platforms. Use FromDIP() for anything window-related
       instead.

        @since 3.1.4
     */
    double GetDPIScaleFactor() const;

    /**
        Returns the size of the left/right and top/bottom borders of this window in x
        and y components of the result respectively.
    */
    virtual wxSize GetWindowBorderSize() const;

    /**
       wxSizer and friends use this to give a chance to a component to recalc
       its min size once one of the final size components is known. Override
       this function when that is useful (such as for wxStaticText which can
       stretch over several lines). Parameter availableOtherDir
       tells the item how much more space there is available in the opposite
       direction (-1 if unknown).
    */
    virtual bool
    InformFirstDirection(int direction,
                         int size,
                         int availableOtherDir);

    /**
        Resets the cached best size value so it will be recalculated the next time it
        is needed.

        @see CacheBestSize()
    */
    void InvalidateBestSize();

    /**
        Posts a size event to the window.

        This is the same as SendSizeEvent() with @c wxSEND_EVENT_POST argument.
     */
    void PostSizeEvent();

    /**
        Posts a size event to the parent of this window.

        This is the same as SendSizeEventToParent() with @c wxSEND_EVENT_POST
        argument.
     */
    void PostSizeEventToParent();

    /**
        This function sends a dummy @ref wxSizeEvent "size event" to
        the window allowing it to re-layout its children positions.

        It is sometimes useful to call this function after adding or deleting a
        children after the frame creation or if a child size changes. Note that
        if the frame is using either sizers or constraints for the children
        layout, it is enough to call wxWindow::Layout() directly and this
        function should not be used in this case.

        If @a flags includes @c wxSEND_EVENT_POST value, this function posts
        the event, i.e. schedules it for later processing, instead of
        dispatching it directly. You can also use PostSizeEvent() as a more
        readable equivalent of calling this function with this flag.

        @param flags
            May include @c wxSEND_EVENT_POST. Default value is 0.
    */
    virtual void SendSizeEvent(int flags = 0);

    /**
        Safe wrapper for GetParent()->SendSizeEvent().

        This function simply checks that the window has a valid parent which is
        not in process of being deleted and calls SendSizeEvent() on it. It is
        used internally by windows such as toolbars changes to whose state
        should result in parent re-layout (e.g. when a toolbar is added to the
        top of the window, all the other windows must be shifted down).

        @see PostSizeEventToParent()

        @param flags
            See description of this parameter in SendSizeEvent() documentation.
     */
    void SendSizeEventToParent(int flags = 0);

    /**
        This sets the size of the window client area in pixels.

        Using this function to size a window tends to be more device-independent
        than SetSize(), since the application need not worry about what dimensions
        the border or title bar have when trying to fit the window around panel
        items, for example.

        @see @ref overview_windowsizing
    */
    void SetClientSize(int width, int height);

    /**
        @overload
    */
    void SetClientSize(const wxSize& size);

    /**
        @overload
    */
    void SetClientSize(const wxRect& rect);

    /**
        Used by wxSizer internally to notify the window about being managed by
        the given sizer.

        This method should not be called from outside the library, unless
        you're implementing a custom sizer class -- and in the latter case you
        must call this method with the pointer to the sizer itself whenever a
        window is added to it and with @NULL argument when the window is
        removed from it.
    */
    void SetContainingSizer(wxSizer* sizer);

    /**
        A @e smart SetSize that will fill in default size components with the
        window's @e best size values.

        Also sets the window's minsize to the value passed in for use with sizers.
        This means that if a full or partial size is passed to this function then
        the sizers will use that size instead of the results of GetBestSize() to
        determine the minimum needs of the window for layout.

        Most controls will use this to set their initial size, and their min
        size to the passed in value (if any.)

        @see SetSize(), GetBestSize(), GetEffectiveMinSize(),
             @ref overview_windowsizing
    */
    void SetInitialSize(const wxSize& size = wxDefaultSize);

    /**
        Sets the maximum client size of the window, to indicate to the sizer
        layout mechanism that this is the maximum possible size of its client area.

        Note that this method is just a shortcut for:
        @code
        SetMaxSize(ClientToWindowSize(size));
        @endcode

        @see SetMaxSize(), @ref overview_windowsizing
    */
    virtual void SetMaxClientSize(const wxSize& size);

    /**
        Sets the maximum size of the window, to indicate to the sizer layout mechanism
        that this is the maximum possible size.

        @see SetMaxClientSize(), @ref overview_windowsizing
    */
    virtual void SetMaxSize(const wxSize& size);

    /**
        Sets the minimum client size of the window, to indicate to the sizer
        layout mechanism that this is the minimum required size of window's client
        area.

        You may need to call this if you change the window size after
        construction and before adding to its parent sizer.

        Note, that just as with SetMinSize(), calling this method doesn't
        prevent the program from explicitly making the window smaller than the
        specified size.

        Note that this method is just a shortcut for:
        @code
        SetMinSize(ClientToWindowSize(size));
        @endcode

        @see SetMinSize(), @ref overview_windowsizing
    */
    virtual void SetMinClientSize(const wxSize& size);

    /**
        Sets the minimum size of the window, to indicate to the sizer layout
        mechanism that this is the minimum required size.

        You may need to call this if you change the window size after
        construction and before adding to its parent sizer.

        Notice that calling this method doesn't prevent the program from making
        the window explicitly smaller than the specified size by calling
        SetSize(), it just ensures that it won't become smaller than this size
        during the automatic layout.

        @see SetMinClientSize(), @ref overview_windowsizing
    */
    virtual void SetMinSize(const wxSize& size);

    /**
        Sets the size of the window in pixels.

        @param x
            Required x position in pixels, or wxDefaultCoord to indicate that the
            existing value should be used.
        @param y
            Required y position in pixels, or wxDefaultCoord to indicate that the
            existing value should be used.
        @param width
            Required width in pixels, or wxDefaultCoord to indicate that the existing
            value should be used.
        @param height
            Required height position in pixels, or wxDefaultCoord to indicate that the
            existing value should be used.
        @param sizeFlags
            Indicates the interpretation of other parameters.
            It is a bit list of the following:
            - @c wxSIZE_AUTO_WIDTH: a wxDefaultCoord width value is taken to indicate
                                    a wxWidgets-supplied default width.
            - @c wxSIZE_AUTO_HEIGHT: a wxDefaultCoord height value is taken to indicate
                                     a wxWidgets-supplied default height.
            - @c wxSIZE_AUTO: wxDefaultCoord size values are taken to indicate
                              a wxWidgets-supplied default size.
            - @c wxSIZE_USE_EXISTING: existing dimensions should be used
                                      if wxDefaultCoord values are supplied.
            - @c wxSIZE_ALLOW_MINUS_ONE: allow negative dimensions (i.e. value of
                                         wxDefaultCoord) to be interpreted as real
                                         dimensions, not default values.
            - @c wxSIZE_FORCE: normally, if the position and the size of the window are
                               already the same as the parameters of this function,
                               nothing is done. but with this flag a window resize may
                               be forced even in this case (supported in wx 2.6.2 and
                               later and only implemented for MSW and ignored elsewhere
                               currently).

        @remarks This overload sets the position and optionally size, of the window.
                 Parameters may be wxDefaultCoord to indicate either that a default
                 should be supplied by wxWidgets, or that the current value of the
                 dimension should be used.

        @see Move(), @ref overview_windowsizing
    */
    void SetSize(int x, int y, int width, int height,
                 int sizeFlags = wxSIZE_AUTO);

    /**
        Sets the size of the window in pixels.
        The size is specified using a wxRect, wxSize or by a couple of @c int objects.

        @remarks This form must be used with non-default width and height values.

        @see Move(), @ref overview_windowsizing
    */
    void SetSize(const wxRect& rect);

    /**
        @overload
    */
    void SetSize(const wxSize& size);

    /**
        @overload
    */
    void SetSize(int width, int height);

    /**
        Use of this function for windows which are not toplevel windows
        (such as wxDialog or wxFrame) is discouraged.
        Please use SetMinSize() and SetMaxSize() instead.

        @see wxTopLevelWindow::SetSizeHints, @ref overview_windowsizing
    */
    virtual void SetSizeHints( const wxSize& minSize,
                               const wxSize& maxSize=wxDefaultSize,
                               const wxSize& incSize=wxDefaultSize);
    /**
        @overload
    */
    virtual void SetSizeHints( int minW, int minH,
                               int maxW = -1, int maxH = -1,
                               int incW = -1, int incH = -1 );

    /**
        Sets the virtual size of the window in pixels.

        @see @ref overview_windowsizing
    */
    void SetVirtualSize(int width, int height);

    /**
        @overload
    */
    void SetVirtualSize(const wxSize& size);

    //@}


    /**
        @name Positioning functions
    */
    //@{

    /**
        A synonym for Centre().
    */
    void Center(int dir = wxBOTH);

    /**
        A synonym for CentreOnParent().
    */
    void CenterOnParent(int dir = wxBOTH);

    /**
        Centres the window.

        @param direction
            Specifies the direction for the centring. May be wxHORIZONTAL, wxVERTICAL
            or wxBOTH. It may also include the wxCENTRE_ON_SCREEN flag
            if you want to centre the window on the entire screen and not on its
            parent window.

        @remarks If the window is a top level one (i.e. doesn't have a parent),
                 it will be centred relative to the screen anyhow.

        @see Center()
    */
    void Centre(int direction = wxBOTH);

    /**
        Centres the window on its parent. This is a more readable synonym for Centre().

        @param direction
            Specifies the direction for the centring. May be wxHORIZONTAL, wxVERTICAL
            or wxBOTH.

        @remarks This methods provides for a way to centre top level windows over
                 their parents instead of the entire screen.  If there
                 is no parent or if the window is not a top level
                 window, then behaviour is the same as Centre().

        @see wxTopLevelWindow::CentreOnScreen
    */
    void CentreOnParent(int direction = wxBOTH);

    /**
        This gets the position of the window in pixels, relative to the parent window
        for the child windows or relative to the display origin for the top level windows.

        @param x
            Receives the x position of the window if non-@NULL.
        @param y
            Receives the y position of the window if non-@NULL.

        @beginWxPerlOnly
        In wxPerl this method is implemented as GetPositionXY() returning
        a 2-element list (x, y).
        @endWxPerlOnly

        @see GetScreenPosition()
    */
    void GetPosition(int* x, int* y) const;

    /**
        This gets the position of the window in pixels, relative to the parent window
        for the child windows or relative to the display origin for the top level windows.

        @see GetScreenPosition()
    */
    wxPoint GetPosition() const;

    /**
        Returns the position and size of the window as a wxRect object.

        @see GetScreenRect()
    */
    wxRect GetRect() const;

    /**
        Returns the window position in screen coordinates, whether the window is a
        child window or a top level one.

        @param x
            Receives the x position of the window on the screen if non-@NULL.
        @param y
            Receives the y position of the window on the screen if non-@NULL.

        @see GetPosition()
    */
    void GetScreenPosition(int* x, int* y) const;

    /**
        Returns the window position in screen coordinates, whether the window is a
        child window or a top level one.

        @see GetPosition()
    */
    wxPoint GetScreenPosition() const;

    /**
        Returns the position and size of the window on the screen as a wxRect object.

        @see GetRect()
    */
    wxRect GetScreenRect() const;

    /**
       Get the origin of the client area of the window relative to the
       window top left corner (the client area may be shifted because of
       the borders, scrollbars, other decorations...)
    */
    virtual wxPoint GetClientAreaOrigin() const;

    /**
       Get the client rectangle in window (i.e.\ client) coordinates
    */
    wxRect GetClientRect() const;



    /**
        Moves the window to the given position.

        @param x
            Required x position.
        @param y
            Required y position.
        @param flags
            See SetSize() for more info about this parameter.

        @remarks Implementations of SetSize can also implicitly implement the
                 Move() function, which is defined in the base wxWindow class as the call:
                 @code
                 SetSize(x, y, wxDefaultCoord, wxDefaultCoord, wxSIZE_USE_EXISTING);
                 @endcode

        @see SetSize()
    */
    void Move(int x, int y, int flags = wxSIZE_USE_EXISTING);

    /**
        Moves the window to the given position.

        @param pt
            wxPoint object representing the position.
        @param flags
            See SetSize() for more info about this parameter.

        @remarks Implementations of SetSize() can also implicitly implement the
                 Move() function, which is defined in the base wxWindow class as the call:
                 @code
                 SetSize(x, y, wxDefaultCoord, wxDefaultCoord, wxSIZE_USE_EXISTING);
                 @endcode

        @see SetSize()
    */
    void Move(const wxPoint& pt, int flags = wxSIZE_USE_EXISTING);

    /**
        Moves the window to the specified position.

        This is exactly the same as calling Move() with the default arguments.
     */
    void SetPosition(const wxPoint& pt);

    //@}


    /**
        @name Coordinate conversion functions
    */
    //@{

    /**
        Converts to screen coordinates from coordinates relative to this window.

        @param x
            A pointer to a integer value for the x coordinate. Pass the client
            coordinate in, and a screen coordinate will be passed out.
        @param y
            A pointer to a integer value for the y coordinate. Pass the client
            coordinate in, and a screen coordinate will be passed out.

        @beginWxPerlOnly
        In wxPerl this method returns a 2-element list instead of
        modifying its parameters.
        @endWxPerlOnly
    */
    void ClientToScreen(int* x, int* y) const;

    /**
        Converts to screen coordinates from coordinates relative to this window.

        @param pt
            The client position for the second form of the function.
    */
    wxPoint ClientToScreen(const wxPoint& pt) const;

    /**
        Converts a point or size from dialog units to pixels.

        For the x dimension, the dialog units are multiplied by the average character
        width and then divided by 4.
        For the y dimension, the dialog units are multiplied by the average character
        height and then divided by 8.

        @remarks Dialog units are used for maintaining a dialog's proportions
                 even if the font changes.
                You can also use these functions programmatically.
                A convenience macro is defined:
                @code
                #define wxDLG_UNIT(parent, pt) parent->ConvertDialogToPixels(pt)
                @endcode

        @see ConvertPixelsToDialog()
    */
    wxPoint ConvertDialogToPixels(const wxPoint& pt) const;

    /**
        @overload
    */
    wxSize ConvertDialogToPixels(const wxSize& sz) const;

    /**
        Converts a point or size from pixels to dialog units.

        For the x dimension, the pixels are multiplied by 4 and then divided by the
        average character width.
        For the y dimension, the pixels are multiplied by 8 and then divided by the
        average character height.

        @remarks Dialog units are used for maintaining a dialog's proportions
                 even if the font changes.

        @see ConvertDialogToPixels()
    */
    wxPoint ConvertPixelsToDialog(const wxPoint& pt) const;

    /**
        @overload
    */
    wxSize ConvertPixelsToDialog(const wxSize& sz) const;

    /**
        Converts from screen to client window coordinates.

        @param x
            Stores the screen x coordinate and receives the client x coordinate.
        @param y
            Stores the screen x coordinate and receives the client x coordinate.
    */
    void ScreenToClient(int* x, int* y) const;

    /**
        Converts from screen to client window coordinates.

        @param pt
            The screen position.
    */
    wxPoint ScreenToClient(const wxPoint& pt) const;

    //@}


    /**
        @name Drawing-related functions
    */
    //@{

    /**
        Clears the window by filling it with the current background colour.

        Does not cause an erase background event to be generated.

        Notice that this uses wxClientDC to draw on the window and the results
        of doing it while also drawing on wxPaintDC for this window are
        undefined. Hence this method shouldn't be used from EVT_PAINT handlers,
        just use wxDC::Clear() on the wxPaintDC you already use there instead.
    */
    virtual void ClearBackground();

    /**
        Freezes the window or, in other words, prevents any updates from taking
        place on screen, the window is not redrawn at all.

        Thaw() must be called to re-enable window redrawing. Calls to these two
        functions may be nested but to ensure that the window is properly
        repainted again, you must thaw it exactly as many times as you froze it.

        If the window has any children, they are recursively frozen too.

        This method is useful for visual appearance optimization (for example,
        it is a good idea to use it before doing many large text insertions in
        a row into a wxTextCtrl under wxGTK) but is not implemented on all
        platforms nor for all controls so it is mostly just a hint to wxWidgets
        and not a mandatory directive.

        @see wxWindowUpdateLocker, Thaw(), IsFrozen()
    */
    void Freeze();

    /**
        Re-enables window updating after a previous call to Freeze().

        To really thaw the control, it must be called exactly the same number
        of times as Freeze().

        If the window has any children, they are recursively thawed too.

        @see wxWindowUpdateLocker, Freeze(), IsFrozen()
    */
    void Thaw();

    /**
        Returns @true if the window is currently frozen by a call to Freeze().

        @see Freeze(), Thaw()
    */
    bool IsFrozen() const;

    /**
        Returns the background colour of the window.

        @see SetBackgroundColour(), SetForegroundColour(), GetForegroundColour()
    */
    wxColour GetBackgroundColour() const;

    /**
        Returns the background style of the window.

        @see SetBackgroundColour(), GetForegroundColour(),
             SetBackgroundStyle(), SetTransparent()
    */
    virtual wxBackgroundStyle GetBackgroundStyle() const;

    /**
        Returns the character height for this window.
    */
    virtual int GetCharHeight() const;

    /**
        Returns the average character width for this window.
    */
    virtual int GetCharWidth() const;

    /**
        Currently this is the same as calling
        wxWindow::GetClassDefaultAttributes(wxWindow::GetWindowVariant()).

        One advantage of using this function compared to the static version is that
        the call is automatically dispatched to the correct class (as usual with
        virtual functions) and you don't have to specify the class name explicitly.

        The other one is that in the future this function could return different
        results, for example it might return a different font for an "Ok" button
        than for a generic button if the users GUI is configured to show such buttons
        in bold font. Of course, the down side is that it is impossible to call this
        function without actually having an object to apply it to whereas the static
        version can be used without having to create an object first.
    */
    virtual wxVisualAttributes GetDefaultAttributes() const;

    /**
        Return the DPI of the display used by this window.

        The returned value can be different for different windows on systems
        with support for per-monitor DPI values, such as Microsoft Windows 10.

        If the DPI is not available, returns @c wxSize(0,0) object.

        @see wxDisplay::GetPPI(), wxDPIChangedEvent

        @since 3.1.3
     */
    virtual wxSize GetDPI() const;

    /**
        Returns the font for this window.

        @see SetFont()
    */
    wxFont GetFont() const;

    /**
        Returns the foreground colour of the window.

        @remarks The meaning of foreground colour varies according to the window class;
                 it may be the text colour or other colour, or it may not be used at all.

        @see SetForegroundColour(), SetBackgroundColour(),
             GetBackgroundColour()
    */
    wxColour GetForegroundColour() const;

    /**
        Gets the dimensions of the string as it would be drawn on the
        window with the currently selected font.

        The text extent is returned in the @a w and @a h pointers.

        @param string
            String whose extent is to be measured.
        @param w
            Return value for width.
        @param h
            Return value for height.
        @param descent
            Return value for descent (optional).
        @param externalLeading
            Return value for external leading (optional).
        @param font
            Font to use instead of the current window font (optional).

        @beginWxPerlOnly
        In wxPerl this method takes only the @a string and optionally
        @a font parameters, and returns a 4-element list
        (x, y, descent, externalLeading).
        @endWxPerlOnly
    */
    void GetTextExtent(const wxString& string,
                       int* w, int* h,
                       int* descent = NULL,
                       int* externalLeading = NULL,
                       const wxFont* font = NULL) const;

    /**
        Gets the dimensions of the string as it would be drawn on the
        window with the currently selected font.
    */
    wxSize GetTextExtent(const wxString& string) const;

    /**
        Returns the region specifying which parts of the window have been damaged.
        Should only be called within an wxPaintEvent handler.

        @see wxRegion, wxRegionIterator
    */
    const wxRegion& GetUpdateRegion() const;

    /**
       Get the update rectangle bounding box in client coords
    */
    wxRect GetUpdateClientRect() const;

    /**
        Returns @true if this window background is transparent (as, for example,
        for wxStaticText) and should show the parent window background.

        This method is mostly used internally by the library itself and you normally
        shouldn't have to call it. You may, however, have to override it in your
        wxWindow-derived class to ensure that background is painted correctly.
    */
    virtual bool HasTransparentBackground();

    /**
        Causes this window, and all of its children recursively (except under wxGTK1
        where this is not implemented), to be repainted. Note that repainting doesn't
        happen immediately but only during the next event loop iteration, if you need
        to update the window immediately you should use Update() instead.

        @param eraseBackground
            If @true, the background will be erased.
        @param rect
            If non-@NULL, only the given rectangle will be treated as damaged.

        @see RefreshRect()
    */
    virtual void Refresh(bool eraseBackground = true,
                         const wxRect* rect = NULL);

    /**
        Redraws the contents of the given rectangle: only the area inside it will be
        repainted.

        This is the same as Refresh() but has a nicer syntax as it can be called
        with a temporary wxRect object as argument like this @c RefreshRect(wxRect(x, y, w, h)).
    */
    void RefreshRect(const wxRect& rect, bool eraseBackground = true);

    /**
        Calling this method immediately repaints the invalidated area of the window and
        all of its children recursively (this normally only happens when the
        flow of control returns to the event loop).

        Notice that this function doesn't invalidate any area of the window so
        nothing happens if nothing has been invalidated (i.e. marked as requiring
        a redraw). Use Refresh() first if you want to immediately redraw the
        window unconditionally.
    */
    virtual void Update();

    /**
        Sets the background colour of the window.

        Notice that as with SetForegroundColour(), setting the background
        colour of a native control may not affect the entire control and could
        be not supported at all depending on the control and platform.

        Please see InheritAttributes() for explanation of the difference between
        this method and SetOwnBackgroundColour().

        @param colour
            The colour to be used as the background colour; pass
            wxNullColour to reset to the default colour.
            Note that you may want to use wxSystemSettings::GetColour() to retrieve
            a suitable colour to use rather than setting an hard-coded one.

        @remarks The background colour is usually painted by the default
                 wxEraseEvent event handler function under Windows and
                 automatically under GTK.
                 Note that setting the background colour does not cause an
                 immediate refresh, so you may wish to call wxWindow::ClearBackground
                 or wxWindow::Refresh after calling this function.
                 Using this function will disable attempts to use themes for
                 this window, if the system supports them. Use with care since
                 usually the themes represent the appearance chosen by the user
                 to be used for all applications on the system.

        @return @true if the colour was really changed, @false if it was already set
                to this colour and nothing was done.

        @see GetBackgroundColour(), SetForegroundColour(),
             GetForegroundColour(), ClearBackground(),
             Refresh(), wxEraseEvent, wxSystemSettings
    */
    virtual bool SetBackgroundColour(const wxColour& colour);

    /**
        Sets the background style of the window.

        The default background style is @c wxBG_STYLE_ERASE which indicates that
        the window background may be erased in @c EVT_ERASE_BACKGROUND handler.
        This is a safe, compatibility default; however you may want to change it
        to @c wxBG_STYLE_SYSTEM if you don't define any erase background event
        handlers at all, to avoid unnecessary generation of erase background
        events and always let system erase the background. And you should
        change the background style to @c wxBG_STYLE_PAINT if you define an
        @c EVT_PAINT handler which completely overwrites the window background as
        in this case erasing it previously, either in @c EVT_ERASE_BACKGROUND
        handler or in the system default handler, would result in flicker as
        the background pixels will be repainted twice every time the window is
        redrawn. Do ensure that the background is entirely erased by your
        @c EVT_PAINT handler in this case however as otherwise garbage may be left
        on screen.

        Notice that in previous versions of wxWidgets a common way to work
        around the above mentioned flickering problem was to define an empty
        @c EVT_ERASE_BACKGROUND handler. Setting background style to
        @c wxBG_STYLE_PAINT is a simpler and more efficient solution to the same
        problem.


        Under wxGTK and wxOSX, you can use ::wxBG_STYLE_TRANSPARENT to obtain
        full transparency of the window background. Note that wxGTK supports
        this only since GTK 2.12 with a compositing manager enabled, call
        IsTransparentBackgroundSupported() to check whether this is the case.

        Also, in order for @c SetBackgroundStyle(wxBG_STYLE_TRANSPARENT) to
        work, it must be called before Create(). If you're using your own
        wxWindow-derived class you should write your code in the following way:
        @code
            class MyWidget : public wxWindow
            {
            public:
                MyWidget(wxWindow* parent, ...)
                    : wxWindow() // Use default ctor here!
                {
                    // Do this first:
                    SetBackgroundStyle(wxBG_STYLE_TRANSPARENT);

                    // And really create the window afterwards:
                    Create(parent, ...);
                }
            };
        @endcode

        @see SetBackgroundColour(), GetForegroundColour(),
             SetTransparent(), IsTransparentBackgroundSupported()
    */
    virtual bool SetBackgroundStyle(wxBackgroundStyle style);

    /**
        Checks whether using transparent background might work.

        If this function returns @false, calling SetBackgroundStyle() with
        ::wxBG_STYLE_TRANSPARENT is not going to work. If it returns @true,
        setting transparent style should normally succeed.

        Notice that this function would typically be called on the parent of a
        window you want to set transparent background style for as the window
        for which this method is called must be fully created.

        @param reason
            If not @NULL, a reason message is provided if transparency is not
            supported.

        @return @true if background transparency is supported.

        @since 2.9.4
    */
    virtual bool IsTransparentBackgroundSupported(wxString *reason = NULL) const;

    /**
        Sets the font for this window. This function should not be called for the
        parent window if you don't want its font to be inherited by its children,
        use SetOwnFont() instead in this case and see InheritAttributes() for more
        explanations.

        Please notice that the given font is not automatically used for
        wxPaintDC objects associated with this window, you need to
        call wxDC::SetFont too. However this font is used by
        any standard controls for drawing their text as well as by
        GetTextExtent().

        @param font
            Font to associate with this window, pass
            wxNullFont to reset to the default font.

        @return @true if the font was really changed, @false if it was already set
                to this font and nothing was done.

        @see GetFont(), InheritAttributes()
    */
    virtual bool SetFont(const wxFont& font);

    /**
        Sets the foreground colour of the window.

        The meaning of foreground colour varies according to the window class;
        it may be the text colour or other colour, or it may not be used at
        all. Additionally, not all native controls support changing their
        foreground colour so this method may change their colour only partially
        or even not at all.

        Please see InheritAttributes() for explanation of the difference between
        this method and SetOwnForegroundColour().

        @param colour
            The colour to be used as the foreground colour; pass
            wxNullColour to reset to the default colour.

        @return @true if the colour was really changed, @false if it was already set
                to this colour and nothing was done.

        @see GetForegroundColour(), SetBackgroundColour(),
             GetBackgroundColour(), ShouldInheritColours()
    */
    virtual bool SetForegroundColour(const wxColour& colour);

    /**
        Sets the background colour of the window but prevents it from being inherited
        by the children of this window.

        @see SetBackgroundColour(), InheritAttributes()
    */
    void SetOwnBackgroundColour(const wxColour& colour);

    /**
        Return @true if this window inherits the background colour from its parent.

        @see SetOwnBackgroundColour(), InheritAttributes()
    */
    bool InheritsBackgroundColour() const;

    /**
        Return @true if a background colour has been set for this window.
    */
    bool UseBgCol() const;

    /**
        Return @true if a background colour has been set for this window.
        Same as @ref UseBgCol()
    */
    bool UseBackgroundColour() const;

    /**
        Sets the font of the window but prevents it from being inherited by the
        children of this window.

        @see SetFont(), InheritAttributes()
    */
    void SetOwnFont(const wxFont& font);

    /**
        Sets the foreground colour of the window but prevents it from being inherited
        by the children of this window.

        @see SetForegroundColour(), InheritAttributes()
    */
    void SetOwnForegroundColour(const wxColour& colour);

    /**
        Return @true if a foreground colour has been set for this window.
    */
    bool UseForegroundColour() const;

    /**
        Return @true if this window inherits the foreground colour from its parent.

        @see SetOwnForegroundColour(), InheritAttributes()
    */
    bool InheritsForegroundColour() const;

    /**
        @deprecated use wxDC::SetPalette instead.
    */
    void SetPalette(const wxPalette& pal);

    /**
        Return @true from here to allow the colours of this window to be changed by
        InheritAttributes(). Returning @false forbids inheriting them from the parent window.

        The base class version returns @false, but this method is overridden in
        wxControl where it returns @true.
    */
    virtual bool ShouldInheritColours() const;

    /**
        This function tells a window if it should use the system's "theme" code
        to draw the windows' background instead of its own background drawing
        code. This does not always have any effect since the underlying platform
        obviously needs to support the notion of themes in user defined windows.
        One such platform is GTK+ where windows can have (very colourful) backgrounds
        defined by a user's selected theme.

        Dialogs, notebook pages and the status bar have this flag set to @true
        by default so that the default look and feel is simulated best.

        @see GetThemeEnabled()
    */
    virtual void SetThemeEnabled(bool enable);

    /**
        Returns @true if the window uses the system theme for drawing its background.

        @see SetThemeEnabled()
     */
    virtual bool GetThemeEnabled() const;

    /**
        Returns @true if the system supports transparent windows and calling
        SetTransparent() may succeed. If this function returns @false, transparent
        windows are definitely not supported by the current system.
    */
    virtual bool CanSetTransparent();

    /**
        Set the transparency of the window. If the system supports transparent windows,
        returns @true, otherwise returns @false and the window remains fully opaque.
        See also CanSetTransparent().

        The parameter @a alpha is in the range 0..255 where 0 corresponds to a
        fully transparent window and 255 to the fully opaque one. The constants
        @c wxIMAGE_ALPHA_TRANSPARENT and @c wxIMAGE_ALPHA_OPAQUE can be used.
    */
    virtual bool SetTransparent(wxByte alpha);

    //@}


    /**
        @name Event-handling functions

        wxWindow allows you to build a (sort of) stack of event handlers which
        can be used to override the window's own event handling.
    */
    //@{

    /**
        Returns the event handler for this window.
        By default, the window is its own event handler.

        @see SetEventHandler(), PushEventHandler(),
             PopEventHandler(), wxEvtHandler::ProcessEvent, wxEvtHandler
    */
    wxEvtHandler* GetEventHandler() const;

    /**
        This function will generate the appropriate call to Navigate() if the key
        event is one normally used for keyboard navigation and return @true in this case.

        @return Returns @true if the key pressed was for navigation and was
                handled, @false otherwise.

        @see Navigate()
    */
    bool HandleAsNavigationKey(const wxKeyEvent& event);

    /**
        Shorthand for:
        @code
        GetEventHandler()->SafelyProcessEvent(event);
        @endcode

        @see ProcessWindowEvent()
    */
    bool HandleWindowEvent(wxEvent& event) const;

    /**
        Convenient wrapper for ProcessEvent().

        This is the same as writing @code GetEventHandler()->ProcessEvent(event);
        @endcode but more convenient. Notice that ProcessEvent() itself can't
        be called for wxWindow objects as it ignores the event handlers
        associated with the window; use this function instead.
    */
    bool ProcessWindowEvent(wxEvent& event);

    /**
        Wrapper for wxEvtHandler::ProcessEventLocally().

        This method is similar to ProcessWindowEvent() but can be used to
        search for the event handler only in this window and any event handlers
        pushed on top of it. Unlike ProcessWindowEvent() it won't propagate the
        event upwards. But it will use the validator and event handlers
        associated with this window, if any.

        @since 2.9.1
     */
    bool ProcessWindowEventLocally(wxEvent& event);

    /**
        Removes and returns the top-most event handler on the event handler stack.

        E.g. in the case of:
            @image html overview_events_winstack.png
        when calling @c W->PopEventHandler(), the event handler @c A will be
        removed and @c B will be the first handler of the stack.

        Note that it's an error to call this function when no event handlers
        were pushed on this window (i.e. when the window itself is its only
        event handler).

        @param deleteHandler
            If this is @true, the handler will be deleted after it is removed
            (and the returned value will be @NULL).

        @see @ref overview_events_processing
    */
    wxEvtHandler* PopEventHandler(bool deleteHandler = false);

    /**
        Pushes this event handler onto the event stack for the window.

        An event handler is an object that is capable of processing the events sent
        to a window. By default, the window is its own event handler, but an application
        may wish to substitute another, for example to allow central implementation
        of event-handling for a variety of different window classes.

        wxWindow::PushEventHandler allows an application to set up a @e stack
        of event handlers, where an event not handled by one event handler is
        handed to the next one in the chain.

        E.g. if you have two event handlers @c A and @c B and a wxWindow instance
        @c W and you call:
        @code
            W->PushEventHandler(A);
            W->PushEventHandler(B);
        @endcode
        you will end up with the following situation:
            @image html overview_events_winstack.png

        Note that you can use wxWindow::PopEventHandler to remove the event handler.

        @param handler
            Specifies the handler to be pushed.
            It must not be part of a wxEvtHandler chain; an assert will fail
            if it's not unlinked (see wxEvtHandler::IsUnlinked).

        @see @ref overview_events_processing
    */
    void PushEventHandler(wxEvtHandler* handler);

    /**
        Find the given @a handler in the windows event handler stack and
        removes (but does not delete) it from the stack.

        See wxEvtHandler::Unlink() for more info.

        @param handler
            The event handler to remove, must be non-@NULL and
            must be present in this windows event handlers stack.

        @return Returns @true if it was found and @false otherwise (this also
                results in an assert failure so this function should
                only be called when the handler is supposed to be there).

        @see PushEventHandler(), PopEventHandler()
    */
    bool RemoveEventHandler(wxEvtHandler* handler);

    /**
        Sets the event handler for this window.

        Note that if you use this function you may want to use as the "next" handler
        of @a handler the window itself; in this way when @a handler doesn't process
        an event, the window itself will have a chance to do it.

        @param handler
            Specifies the handler to be set. Cannot be @NULL.

        @see @ref overview_events_processing
    */
    void SetEventHandler(wxEvtHandler* handler);

    /**
        wxWindows cannot be used to form event handler chains; this function
        thus will assert when called.

        Note that instead you can use PushEventHandler() or SetEventHandler() to
        implement a stack of event handlers to override wxWindow's own
        event handling mechanism.
    */
    virtual void SetNextHandler(wxEvtHandler* handler);

    /**
        wxWindows cannot be used to form event handler chains; this function
        thus will assert when called.

        Note that instead you can use PushEventHandler() or SetEventHandler() to
        implement a stack of event handlers to override wxWindow's own
        event handling mechanism.
    */
    virtual void SetPreviousHandler(wxEvtHandler* handler);

    //@}



    /**
        @name Window styles functions
    */
    //@{

    /**
        Returns the extra style bits for the window.
    */
    long GetExtraStyle() const;

    /**
        Gets the window style that was passed to the constructor or Create()
        method. GetWindowStyle() is another name for the same function.
    */
    virtual long GetWindowStyleFlag() const;

    /**
        See GetWindowStyleFlag() for more info.
    */
    long GetWindowStyle() const;

    /**
        Returns @true if the window has the given @a exFlag bit set in its
        extra styles.

        @see SetExtraStyle()
    */
    bool HasExtraStyle(int exFlag) const;

    /**
        Returns @true if the window has the given @a flag bit set.
    */
    bool HasFlag(int flag) const;

    /**
        Sets the extra style bits for the window.
        The currently defined extra style bits are reported in the class
        description.
    */
    virtual void SetExtraStyle(long exStyle);

    /**
        Sets the style of the window. Please note that some styles cannot be changed
        after the window creation and that Refresh() might need to be called
        after changing the others for the change to take place immediately.

        See @ref overview_windowstyles "Window styles" for more information about flags.

        @see GetWindowStyleFlag()
    */
    virtual void SetWindowStyleFlag(long style);

    /**
        See SetWindowStyleFlag() for more info.
    */
    void SetWindowStyle(long style);

    /**
        Turns the given @a flag on if it's currently turned off and vice versa.
        This function cannot be used if the value of the flag is 0 (which is often
        the case for default flags).

        Also, please notice that not all styles can be changed after the control
        creation.

        @return Returns @true if the style was turned on by this function, @false
                 if it was switched off.

        @see SetWindowStyleFlag(), HasFlag()
    */
    bool ToggleWindowStyle(int flag);

    //@}


    /**
        @name Tab order functions
    */
    //@{

    /**
        Moves this window in the tab navigation order after the specified @e win.
        This means that when the user presses @c TAB key on that other window,
        the focus switches to this window.

        Default tab order is the same as creation order, this function and
        MoveBeforeInTabOrder() allow to change
        it after creating all the windows.

        @param win
            A sibling of this window which should precede it in tab order,
            must not be @NULL
    */
    void MoveAfterInTabOrder(wxWindow* win);

    /**
        Same as MoveAfterInTabOrder() except that it inserts this window just
        before @a win instead of putting it right after it.
    */
    void MoveBeforeInTabOrder(wxWindow* win);

    /**
        Performs a keyboard navigation action starting from this window.
        This method is equivalent to calling NavigateIn() method on the
        parent window.

        @param flags
            A combination of wxNavigationKeyEvent::IsForward and
            wxNavigationKeyEvent::WinChange.

        @return Returns @true if the focus was moved to another window or @false
                if nothing changed.

        @remarks You may wish to call this from a text control custom keypress
                 handler to do the default navigation behaviour for the
                 tab key, since the standard default behaviour for a
                 multiline text control with the wxTE_PROCESS_TAB style
                 is to insert a tab and not navigate to the next
                 control. See also wxNavigationKeyEvent and
                 HandleAsNavigationKey.
    */
    bool Navigate(int flags = wxNavigationKeyEvent::IsForward);

    /**
        Performs a keyboard navigation action inside this window.
        See Navigate() for more information.
    */
    bool NavigateIn(int flags = wxNavigationKeyEvent::IsForward);

    //@}



    /**
        @name Z order functions
    */
    //@{

    /**
        Lowers the window to the bottom of the window hierarchy (Z-order).

        @remarks
        This function only works for wxTopLevelWindow-derived classes.

        @see Raise()
    */
    virtual void Lower();

    /**
        Raises the window to the top of the window hierarchy (Z-order).

        Notice that this function only requests the window manager to raise
        this window to the top of Z-order. Depending on its configuration, the
        window manager may raise the window, not do it at all or indicate that
        a window requested to be raised in some other way, e.g. by flashing its
        icon if it is minimized.

        @remarks
        This function only works for wxTopLevelWindow-derived classes.

        @see Lower()
    */
    virtual void Raise();

    //@}


    /**
        @name Window status functions
    */
    //@{


    /**
        Equivalent to calling wxWindow::Show(@false).
    */
    bool Hide();

    /**
        This function hides a window, like Hide(), but using a special visual
        effect if possible.

        The parameters of this function are the same as for ShowWithEffect(),
        please see their description there.

        @since 2.9.0
    */
    virtual bool HideWithEffect(wxShowEffect effect,
                                unsigned int timeout = 0);
    /**
        Returns @true if the window is enabled, i.e.\ if it accepts user input,
        @false otherwise.

        Notice that this method can return @false even if this window itself hadn't
        been explicitly disabled when one of its parent windows is disabled.
        To get the intrinsic status of this window, use IsThisEnabled()

        @see Enable()
    */
    bool IsEnabled() const;

    /**
        Returns @true if the given point or rectangle area has been exposed since the
        last repaint. Call this in an paint event handler to optimize redrawing by
        only redrawing those areas, which have been exposed.
    */
    bool IsExposed(int x, int y) const;

    /**
        @overload
    */
    bool IsExposed(wxPoint& pt) const;

    /**
        @overload
    */
    bool IsExposed(int x, int y, int w, int h) const;

    /**
        @overload
    */
    bool IsExposed(wxRect& rect) const;
    /**
        Returns @true if the window is shown, @false if it has been hidden.

        @see IsShownOnScreen()
    */
    virtual bool IsShown() const;

    /**
        Returns @true if the window is physically visible on the screen, i.e.\ it
        is shown and all its parents up to the toplevel window are shown as well.

        @see IsShown()
    */
    virtual bool IsShownOnScreen() const;

    /**
        Disables the window. Same as @ref Enable() Enable(@false).

        @return Returns @true if the window has been disabled, @false if it had
                been already disabled before the call to this function.
    */
    bool Disable();

    /**
        Enable or disable the window for user input. Note that when a parent window is
        disabled, all of its children are disabled as well and they are re-enabled again
        when the parent is.

        A window can be created initially disabled by calling this method on it
        @e before calling Create() to create the actual underlying window, e.g.
        @code
            wxWindow* w = new MyWindow(); // Note: default ctor is used here.
            w->Enable(false);
            w->Create(parent, ... all the usual non-default ctor arguments ...);
        @endcode

        @param enable
            If @true, enables the window for input. If @false, disables the window.

        @return Returns @true if the window has been enabled or disabled, @false
                if nothing was done, i.e. if the window had already
                been in the specified state.

        @see IsEnabled(), Disable(), wxRadioBox::Enable
    */
    virtual bool Enable(bool enable = true);

    /**
        Shows or hides the window. You may need to call Raise()
        for a top level window if you want to bring it to top, although this is not
        needed if Show() is called immediately after the frame creation.

        Notice that the default state of newly created top level windows is hidden
        (to allow you to create their contents without flicker) unlike for
        all the other, not derived from wxTopLevelWindow, windows that
        are by default created in the shown state.

        @param show
            If @true displays the window. Otherwise, hides it.

        @return @true if the window has been shown or hidden or @false if nothing
                 was done because it already was in the requested state.

        @see IsShown(), Hide(), wxRadioBox::Show, wxShowEvent.
    */
    virtual bool Show(bool show = true);

    /**
        This function shows a window, like Show(), but using a special visual
        effect if possible.

        @param effect
            The effect to use.

        @param timeout
            The @a timeout parameter specifies the time of the animation, in
            milliseconds. If the default value of 0 is used, the default
            animation time for the current platform is used.

        @note Currently this function is only implemented in wxMSW and wxOSX
              and does the same thing as Show() in the other ports.

        @since 2.9.0

        @see HideWithEffect()
    */
    virtual bool ShowWithEffect(wxShowEffect effect,
                                unsigned int timeout = 0);

    //@}


    /**
        @name Context-sensitive help functions
    */
    //@{

    /**
        Gets the help text to be used as context-sensitive help for this window.
        Note that the text is actually stored by the current wxHelpProvider
        implementation, and not in the window object itself.

        @see SetHelpText(), GetHelpTextAtPoint(), wxHelpProvider
    */
    wxString GetHelpText() const;

    /**
        Sets the help text to be used as context-sensitive help for this window.
        Note that the text is actually stored by the current wxHelpProvider
        implementation, and not in the window object itself.

        @see GetHelpText(), wxHelpProvider::AddHelp()
    */
    void SetHelpText(const wxString& helpText);

    /**
        Gets the help text to be used as context-sensitive help for this window.
        This method should be overridden if the help message depends on the position
        inside the window, otherwise GetHelpText() can be used.

        @param point
            Coordinates of the mouse at the moment of help event emission.
        @param origin
            Help event origin, see also wxHelpEvent::GetOrigin.
    */
    virtual wxString GetHelpTextAtPoint(const wxPoint& point,
                                        wxHelpEvent::Origin origin) const;

    /**
        Get the associated tooltip or @NULL if none.
    */
    wxToolTip* GetToolTip() const;

    /**
        Get the text of the associated tooltip or empty string if none.
     */
    wxString GetToolTipText() const;

    /**
        Attach a tooltip to the window.

        wxToolTip pointer can be @NULL in the overload taking the pointer,
        meaning to unset any existing tooltips; however UnsetToolTip() provides
        a more readable alternative to this operation.

        Notice that these methods are always available, even if wxWidgets was
        compiled with @c wxUSE_TOOLTIPS set to 0, but don't do anything in this
        case.

        @see GetToolTip(), wxToolTip
    */
    void SetToolTip(const wxString& tipString);

    /**
        @overload
    */
    void SetToolTip(wxToolTip* tip);

    /**
        Unset any existing tooltip.

        @since 2.9.0

        @see SetToolTip()
     */
    void UnsetToolTip();

    //@}


    /**
        @name Popup/context menu functions
    */
    //@{

    /**
        This function shows a popup menu at the given position in this window and
        returns the selected id.

        It can be more convenient than the general purpose PopupMenu() function
        for simple menus proposing a choice in a list of strings to the user.

        Notice that to avoid unexpected conflicts between the (usually
        consecutive range of) ids used by the menu passed to this function and
        the existing EVT_UPDATE_UI() handlers, this function temporarily
        disables UI updates for the window, so you need to manually disable
        (or toggle or ...) any items which should be disabled in the menu
        before showing it.

        The parameter @a menu is the menu to show.
        The parameter @a pos (or the parameters @a x and @a y) is the
        position at which to show the menu in client coordinates.
        It is recommended to not explicitly specify coordinates when
        calling this method in response to mouse click, because some of
        the ports (namely, wxGTK) can do a better job of positioning
        the menu in that case.

        @return
             The selected menu item id or @c wxID_NONE if none selected or an
             error occurred.

        @since 2.9.0
    */
    int GetPopupMenuSelectionFromUser(wxMenu& menu,
                                      const wxPoint& pos = wxDefaultPosition);

    /**
        @overload
    */
    int GetPopupMenuSelectionFromUser(wxMenu& menu, int x, int y);

    /**
        Pops up the given menu at the specified coordinates, relative to this
        window, and returns control when the user has dismissed the menu.

        If a menu item is selected, the corresponding menu event is generated and will be
        processed as usual. If coordinates are not specified, the current mouse
        cursor position is used.

        @a menu is the menu to pop up.

        The position where the menu will appear can be specified either as a
        wxPoint @a pos or by two integers (@a x and @a y).

        Note that this function switches focus to this window before showing
        the menu.

        @remarks Just before the menu is popped up, wxMenu::UpdateUI is called to
                 ensure that the menu items are in the correct state.
                 The menu does not get deleted by the window.
                 It is recommended to not explicitly specify coordinates when
                 calling PopupMenu in response to mouse click, because some of
                 the ports (namely, wxGTK) can do a better job of positioning
                 the menu in that case.

        @see wxMenu
    */
    bool PopupMenu(wxMenu* menu,
                   const wxPoint& pos = wxDefaultPosition);

    /**
        @overload
    */
    bool PopupMenu(wxMenu* menu, int x, int y);

    //@}


    /**
        Validator functions
    */
    //@{

    /**
        Returns a pointer to the current validator for the window, or @NULL if
        there is none.
    */
    virtual wxValidator* GetValidator();

    /**
        Deletes the current validator (if any) and sets the window validator, having
        called wxValidator::Clone to create a new validator of this type.
    */
    virtual void SetValidator(const wxValidator& validator);

    /**
        Transfers values from child controls to data areas specified by their
        validators. Returns @false if a transfer failed.

        Notice that this also calls TransferDataFromWindow() for all children
        recursively.

        @see TransferDataToWindow(), wxValidator, Validate()
    */
    virtual bool TransferDataFromWindow();

    /**
        Transfers values to child controls from data areas specified by their
        validators.

        Notice that this also calls TransferDataToWindow() for all children
        recursively.

        @return Returns @false if a transfer failed.

        @see TransferDataFromWindow(), wxValidator, Validate()
    */
    virtual bool TransferDataToWindow();

    /**
        Validates the current values of the child controls using their validators.

        Notice that this also calls Validate() for all children recursively.

        @return Returns @false if any of the validations failed.

        @see TransferDataFromWindow(), TransferDataToWindow(),
             wxValidator
    */
    virtual bool Validate();

    //@}


    /**
        @name wxWindow properties functions
    */
    //@{

    /**
        Returns the identifier of the window.

        @remarks Each window has an integer identifier. If the application
                 has not provided one (or the default wxID_ANY) a unique
                 identifier with a negative value will be generated.

        @see SetId(), @ref overview_windowids
    */
    wxWindowID GetId() const;

    /**
        Generic way of getting a label from any window, for
        identification purposes.

        @remarks The interpretation of this function differs from class to class.
                 For frames and dialogs, the value returned is the
                 title. For buttons or static text controls, it is the
                 button text. This function can be useful for
                 meta-programs (such as testing tools or special-needs
                 access programs) which need to identify windows by name.
    */
    virtual wxString GetLabel() const;

    /**
        Returns the layout direction for this window,
        Note that @c wxLayout_Default is returned if layout direction is not supported.
    */
    virtual wxLayoutDirection GetLayoutDirection() const;

    /**
       Mirror coordinates for RTL layout if this window uses it and if the
       mirroring is not done automatically like Win32.
    */
    virtual wxCoord AdjustForLayoutDirection(wxCoord x,
                                             wxCoord width,
                                             wxCoord widthTotal) const;

    /**
        Returns the window's name.

        @remarks This name is not guaranteed to be unique; it is up to the
                 programmer to supply an appropriate name in the window
                 constructor or via SetName().

        @see SetName()
    */
    virtual wxString GetName() const;

    /**
        Returns the value previously passed to SetWindowVariant().
    */
    wxWindowVariant GetWindowVariant() const;

    /**
        Sets the identifier of the window.

        @remarks Each window has an integer identifier. If the application has
                 not provided one, an identifier will be generated.
                 Normally, the identifier should be provided on creation
                 and should not be modified subsequently.

        @see GetId(), @ref overview_windowids
    */
    void SetId(wxWindowID winid);

    /**
        Sets the window's label.

        @param label
            The window label.

        @see GetLabel()
    */
    virtual void SetLabel(const wxString& label);

    /**
        Sets the layout direction for this window.

        This function is only supported under MSW and GTK platforms, but not
        under Mac currently.
    */
    virtual void SetLayoutDirection(wxLayoutDirection dir);

    /**
        Sets the window's name.

        @param name
            A name to set for the window.

        @see GetName()
    */
    virtual void SetName(const wxString& name);

    /**
        Chooses a different variant of the window display to use.

        Window variants currently just differ in size, as can be seen from
        ::wxWindowVariant documentation. Under all platforms but macOS, this
        function does nothing more than change the font used by the window.
        However under macOS it is implemented natively and selects the
        appropriate variant of the native widget, which has better appearance
        than just scaled down or up version of the normal variant, so it should
        be preferred to directly tweaking the font size.

        By default the controls naturally use the normal variant.
    */
    void SetWindowVariant(wxWindowVariant variant);

    /**
        Gets the accelerator table for this window. See wxAcceleratorTable.
    */
    wxAcceleratorTable* GetAcceleratorTable();

    /**
        Returns the accessible object for this window, if any.
        See also wxAccessible.
    */
    wxAccessible* GetAccessible();

    /**
        Sets the accelerator table for this window. See wxAcceleratorTable.
    */
    virtual void SetAcceleratorTable(const wxAcceleratorTable& accel);

    /**
        Sets the accessible for this window. Any existing accessible for this window
        will be deleted first, if not identical to @e accessible.
        See also wxAccessible.
    */
    void SetAccessible(wxAccessible* accessible);

    //@}


    /**
        @name Window deletion functions
    */
    //@{

    /**
        This function simply generates a wxCloseEvent whose handler usually tries
        to close the window. It doesn't close the window itself, however.

        @param force
            @false if the window's close handler should be able to veto the destruction
            of this window, @true if it cannot.

        @return @true if the event was handled and not vetoed, @false otherwise.

        @remarks Close calls the close handler for the window, providing an
                 opportunity for the window to choose whether to destroy
                 the window. Usually it is only used with the top level
                 windows (wxFrame and wxDialog classes) as the others
                 are not supposed to have any special OnClose() logic.
                The close handler should check whether the window is being deleted
                forcibly, using wxCloseEvent::CanVeto, in which case it should
                destroy the window using wxWindow::Destroy.
                Note that calling Close does not guarantee that the window will
                be destroyed; but it provides a way to simulate a manual close
                of a window, which may or may not be implemented by destroying
                the window. The default implementation of wxDialog::OnCloseWindow
                does not necessarily delete the dialog, since it will simply
                simulate an wxID_CANCEL event which is handled by the appropriate
                button event handler and may do anything at all.
                To guarantee that the window will be destroyed, call
                wxWindow::Destroy instead

        @see @ref overview_windowdeletion "Window Deletion Overview",
             Destroy(), wxCloseEvent
    */
    bool Close(bool force = false);

    /**
        Destroys the window safely. Use this function instead of the delete operator,
        since different window classes can be destroyed differently. Frames and dialogs
        are not destroyed immediately when this function is called -- they are added
        to a list of windows to be deleted on idle time, when all the window's events
        have been processed. This prevents problems with events being sent to
        non-existent windows.

        @return @true if the window has either been successfully deleted, or it
                 has been added to the list of windows pending real deletion.
    */
    virtual bool Destroy();

    /**
        Returns true if this window is in process of being destroyed.

        Top level windows are not deleted immediately but are rather
        scheduled for later destruction to give them time to process any
        pending messages; see Destroy() description.

        This function returns @true if this window, or one of its parent
        windows, is scheduled for destruction and can be useful to avoid
        manipulating it as it's usually useless to do something with a window
        which is at the point of disappearing anyhow.
     */
    bool IsBeingDeleted() const;

    //@}



    /**
        @name Drag and drop functions
    */
    //@{

    /**
        Returns the associated drop target, which may be @NULL.

        @see SetDropTarget(), @ref overview_dnd
    */
    virtual wxDropTarget* GetDropTarget() const;

    /**
        Associates a drop target with this window.
        If the window already has a drop target, it is deleted.

        @see GetDropTarget(), @ref overview_dnd
    */
    virtual void SetDropTarget(wxDropTarget* target);

    /**
        Enables or disables eligibility for drop file events (OnDropFiles).

        @param accept
            If @true, the window is eligible for drop file events.
            If @false, the window will not accept drop file events.

        @remarks Windows only until version 2.8.9, available on all platforms
                 since 2.8.10. Cannot be used together with SetDropTarget() on
                 non-Windows platforms.

        @see SetDropTarget()
    */
    virtual void DragAcceptFiles(bool accept);

    //@}


    /**
        @name Constraints, sizers and window layout functions
    */
    //@{

    /**
        Returns the sizer of which this window is a member, if any, otherwise @NULL.
    */
    wxSizer* GetContainingSizer() const;

    /**
        Returns the sizer associated with the window by a previous call to
        SetSizer(), or @NULL.
    */
    wxSizer* GetSizer() const;

    /**
        Sets the window to have the given layout sizer.

        The window will then own the object, and will take care of its deletion.
        If an existing layout constraints object is already owned by the
        window, it will be deleted if the @a deleteOld parameter is @true.

        Note that this function will also call SetAutoLayout() implicitly with @true
        parameter if the @a sizer is non-@NULL and @false otherwise so that the
        sizer will be effectively used to layout the window children whenever
        it is resized.

        @param sizer
            The sizer to set. Pass @NULL to disassociate and conditionally delete
            the window's sizer. See below.
        @param deleteOld
            If @true (the default), this will delete any pre-existing sizer.
            Pass @false if you wish to handle deleting the old sizer yourself
            but remember to do it yourself in this case to avoid memory leaks.

        @remarks SetSizer enables and disables Layout automatically.
    */
    void SetSizer(wxSizer* sizer, bool deleteOld = true);

    /**
        Associate the sizer with the window and set the window size and minimal
        size accordingly.

        This method calls SetSizer() and then wxSizer::SetSizeHints() which
        sets the initial window size to the size needed to accommodate all
        sizer elements and sets the minimal size to the same size, this
        preventing the user from resizing this window to be less than this
        minimal size (if it's a top-level window which can be directly resized
        by the user).
    */
    void SetSizerAndFit(wxSizer* sizer, bool deleteOld = true);

    /**
        Returns a pointer to the window's layout constraints, or @NULL if there are none.
    */
    wxLayoutConstraints* GetConstraints() const;

    /**
        Sets the window to have the given layout constraints. The window
        will then own the object, and will take care of its deletion.
        If an existing layout constraints object is already owned by the
        window, it will be deleted.

        @param constraints
            The constraints to set. Pass @NULL to disassociate and delete the window's
            constraints.

        @remarks You must call SetAutoLayout() to tell a window to use
                 the constraints automatically in OnSize; otherwise, you
                 must override OnSize and call Layout() explicitly. When
                 setting both a wxLayoutConstraints and a wxSizer, only
                 the sizer will have effect.
    */
    void SetConstraints(wxLayoutConstraints* constraints);

    /**
        Lays out the children of this window using the associated sizer.

        If a sizer hadn't been associated with this window (see SetSizer()),
        this function doesn't do anything, unless this is a top level window
        (see wxTopLevelWindow::Layout()).

        Note that this method is called automatically when the window size
        changes if it has the associated sizer (or if SetAutoLayout() with
        @true argument had been explicitly called), ensuring that it is always
        laid out correctly.

        @see @ref overview_windowsizing

        @returns Always returns @true, the return value is not useful.
    */
    virtual bool Layout();

    /**
        Determines whether the Layout() function will be called automatically
        when the window is resized.

        This method is called implicitly by SetSizer() but if you use SetConstraints()
        you should call it manually or otherwise the window layout won't be correctly
        updated when its size changes.

        @param autoLayout
            Set this to @true if you wish the Layout() function to be called
            automatically when the window is resized.

        @see SetSizer(), SetConstraints()
    */
    void SetAutoLayout(bool autoLayout);

    bool GetAutoLayout() const;

    //@}



    /**
        @name Mouse functions
    */
    //@{

    /**
        Directs all mouse input to this window.
        Call ReleaseMouse() to release the capture.

        Note that wxWidgets maintains the stack of windows having captured the mouse
        and when the mouse is released the capture returns to the window which had had
        captured it previously and it is only really released if there were no previous
        window. In particular, this means that you must release the mouse as many times
        as you capture it, unless the window receives the wxMouseCaptureLostEvent event.

        Any application which captures the mouse in the beginning of some operation
        must handle wxMouseCaptureLostEvent and cancel this operation when it receives
        the event. The event handler must not recapture mouse.

        @see ReleaseMouse(), wxMouseCaptureLostEvent
    */
    void CaptureMouse();

    /**
        Returns the caret() associated with the window.
    */
    wxCaret* GetCaret() const;

    /**
        Return the cursor associated with this window.

        @see SetCursor()
    */
    const wxCursor& GetCursor() const;

    /**
        Returns @true if this window has the current mouse capture.

        @see CaptureMouse(), ReleaseMouse(), wxMouseCaptureLostEvent,
             wxMouseCaptureChangedEvent
    */
    virtual bool HasCapture() const;

    /**
        Releases mouse input captured with CaptureMouse().

        @see CaptureMouse(), HasCapture(), ReleaseMouse(),
             wxMouseCaptureLostEvent, wxMouseCaptureChangedEvent
    */
    void ReleaseMouse();

    /**
        Sets the caret() associated with the window.
    */
    void SetCaret(wxCaret* caret);

    /**
        Sets the window's cursor. Notice that the window cursor also sets it for the
        children of the window implicitly.

        The @a cursor may be @c wxNullCursor in which case the window cursor will
        be reset back to default.

        @param cursor
            Specifies the cursor that the window should normally display.

        @see ::wxSetCursor, wxCursor
    */
    virtual bool SetCursor(const wxCursor& cursor);

    /**
        Moves the pointer to the given position on the window.

        @note Apple Human Interface Guidelines forbid moving the mouse cursor
              programmatically so you should avoid using this function in Mac
              applications (and probably avoid using it under the other
              platforms without good reason as well).

        @param x
            The new x position for the cursor.
        @param y
            The new y position for the cursor.
    */
    virtual void WarpPointer(int x, int y);

    /**
        Request generation of touch events for this window.

        Each call to this function supersedes the previous ones, i.e. if you
        want to receive events for both zoom and rotate gestures, you need to
        call
        @code
            EnableTouchEvents(wxTOUCH_ZOOM_GESTURE | wxTOUCH_ROTATE_GESTURE);
        @endcode
        instead of calling it twice in a row as the second call would disable
        the first gesture.

        @param eventsMask Either wxTOUCH_NONE or wxTOUCH_ALL_GESTURES to
            disable or enable gesture events for this window.

        @return @true if the specified events were enabled or @false if the
            current platform doesn't support touch events.

        @since 3.1.1
     */
    virtual bool EnableTouchEvents(int eventsMask);

    //@}




    /**
        @name Miscellaneous functions
    */
    //@{

    /**
        Return where the given point lies, exactly.

        This method is used to test whether the point lies inside the client
        window area or on one of its scrollbars.

        The point coordinates are specified in client window coordinates.
     */
    wxHitTest HitTest(wxCoord x, wxCoord y) const;

    /**
        @overload
     */
    wxHitTest HitTest(const wxPoint& pt) const;

    /**
       Get the window border style from the given flags: this is different from
       simply doing flags & wxBORDER_MASK because it uses GetDefaultBorder() to
       translate wxBORDER_DEFAULT to something reasonable
    */
    wxBorder GetBorder(long flags) const;

    /**
       Get border for the flags of this window
    */
    wxBorder GetBorder() const;


    /**
        Does the window-specific updating after processing the update event.
        This function is called by UpdateWindowUI() in order to check return
        values in the wxUpdateUIEvent and act appropriately.
        For example, to allow frame and dialog title updating, wxWidgets
        implements this function as follows:

        @code
        // do the window-specific processing after processing the update event
        void wxTopLevelWindowBase::DoUpdateWindowUI(wxUpdateUIEvent& event)
        {
            if ( event.GetSetEnabled() )
                Enable(event.GetEnabled());

            if ( event.GetSetText() )
            {
                if ( event.GetText() != GetTitle() )
                    SetTitle(event.GetText());
            }
        }
        @endcode
    */
    virtual void DoUpdateWindowUI(wxUpdateUIEvent& event);

    /**
        Returns the platform-specific handle of the physical window.
        Cast it to an appropriate handle, such as @b HWND for Windows,
        @b Widget for Motif or @b GtkWidget for GTK.

        @beginWxPerlOnly
        This method will return an integer in wxPerl.
        @endWxPerlOnly
    */
    virtual WXWidget GetHandle() const;

    /**
        This method should be overridden to return @true if this window has
        multiple pages. All standard class with multiple pages such as
        wxNotebook, wxListbook and wxTreebook already override it to return @true
        and user-defined classes with similar behaviour should also do so, to
        allow the library to handle such windows appropriately.
    */
    virtual bool HasMultiplePages() const;

    /**
        This function is (or should be, in case of custom controls) called during
        window creation to intelligently set up the window visual attributes, that is
        the font and the foreground and background colours.

        By "intelligently" the following is meant: by default, all windows use their
        own @ref GetClassDefaultAttributes() default attributes.
        However if some of the parents attributes are explicitly (that is, using
        SetFont() and not wxWindow::SetOwnFont) changed and if the corresponding
        attribute hadn't been explicitly set for this window itself, then this
        window takes the same value as used by the parent.
        In addition, if the window overrides ShouldInheritColours() to return @false,
        the colours will not be changed no matter what and only the font might.

        This rather complicated logic is necessary in order to accommodate the
        different usage scenarios. The most common one is when all default attributes
        are used and in this case, nothing should be inherited as in modern GUIs
        different controls use different fonts (and colours) than their siblings so
        they can't inherit the same value from the parent. However it was also deemed
        desirable to allow to simply change the attributes of all children at once by
        just changing the font or colour of their common parent, hence in this case we
        do inherit the parents attributes.
    */
    virtual void InheritAttributes();

    /**
        Sends an @c wxEVT_INIT_DIALOG event, whose handler usually transfers data
        to the dialog via validators.
    */
    virtual void InitDialog();

    /**
        Returns @true if the window contents is double-buffered by the system, i.e.\ if
        any drawing done on the window is really done on a temporary backing surface
        and transferred to the screen all at once later.

        @see wxBufferedDC
    */
    virtual bool IsDoubleBuffered() const;

    /**
       Turn on or off double buffering of the window if the system supports it.
    */
    void SetDoubleBuffered(bool on);

    /**
        Returns @true if the window is retained, @false otherwise.

        @remarks Retained windows are only available on X platforms.
    */
    virtual bool IsRetained() const;

    /**
        Returns @true if this window is intrinsically enabled, @false otherwise,
        i.e.\ if @ref Enable() Enable(@false) had been called. This method is
        mostly used for wxWidgets itself, user code should normally use
        IsEnabled() instead.
    */
    bool IsThisEnabled() const;

    /**
        Returns @true if the given window is a top-level one. Currently all frames and
        dialogs are considered to be top-level windows (even if they have a parent
        window).
    */
    virtual bool IsTopLevel() const;


    /**
        This virtual function is normally only used internally, but
        sometimes an application may need it to implement functionality
        that should not be disabled by an application defining an OnIdle
        handler in a derived class.

        This function may be used to do delayed painting, for example,
        and most implementations call UpdateWindowUI()
        in order to send update events to the window in idle time.
    */
    virtual void OnInternalIdle();

    /**
       Send idle event to window and all subwindows. Returns true if more idle
       time is requested.
    */
    virtual bool SendIdleEvents(wxIdleEvent& event);

    /**
        Registers a system wide hotkey. Every time the user presses the hotkey
        registered here, this window will receive a hotkey event.

        It will receive the event even if the application is in the background
        and does not have the input focus because the user is working with some
        other application.

        @param hotkeyId
            Numeric identifier of the hotkey. For applications this must be between 0
            and 0xBFFF. If this function is called from a shared DLL, it must be a
            system wide unique identifier between 0xC000 and 0xFFFF.
            This is a MSW specific detail.
        @param modifiers
            A bitwise combination of wxMOD_SHIFT, wxMOD_CONTROL, wxMOD_ALT
            or wxMOD_WIN specifying the modifier keys that have to be pressed along
            with the key.
        @param virtualKeyCode
            The key code of the hotkey, e.g. an ASCII character such as @c 'K'
            or one of elements of wxKeyCode enum.

        @return @true if the hotkey was registered successfully. @false if some
                 other application already registered a hotkey with this
                 modifier/virtualKeyCode combination.

        @remarks Use EVT_HOTKEY(hotkeyId, fnc) in the event table to capture the
                 event. This function is currently only implemented under MSW
                 and macOS and always returns false in the other ports.

        @see UnregisterHotKey()
    */
    virtual bool RegisterHotKey(int hotkeyId, int modifiers,
                                int virtualKeyCode);

    /**
        Unregisters a system wide hotkey.

        @param hotkeyId
            Numeric identifier of the hotkey. Must be the same id that was passed to
            RegisterHotKey().

        @return @true if the hotkey was unregistered successfully, @false if the
                id was invalid.

        @remarks This function is currently only implemented under MSW.

        @see RegisterHotKey()
    */
    virtual bool UnregisterHotKey(int hotkeyId);

    /**
        This function sends one or more wxUpdateUIEvent to the window.
        The particular implementation depends on the window; for example a
        wxToolBar will send an update UI event for each toolbar button,
        and a wxFrame will send an update UI event for each menubar menu item.

        You can call this function from your application to ensure that your
        UI is up-to-date at this point (as far as your wxUpdateUIEvent handlers
        are concerned). This may be necessary if you have called
        wxUpdateUIEvent::SetMode() or wxUpdateUIEvent::SetUpdateInterval() to limit
        the overhead that wxWidgets incurs by sending update UI events in idle time.
        @a flags should be a bitlist of one or more of the ::wxUpdateUI enumeration.

        If you are calling this function from an OnInternalIdle or OnIdle
        function, make sure you pass the wxUPDATE_UI_FROMIDLE flag, since
        this tells the window to only update the UI elements that need
        to be updated in idle time. Some windows update their elements
        only when necessary, for example when a menu is about to be shown.
        The following is an example of how to call UpdateWindowUI from
        an idle function.

        @code
        void MyWindow::OnInternalIdle()
        {
            if (wxUpdateUIEvent::CanUpdate(this))
                UpdateWindowUI(wxUPDATE_UI_FROMIDLE);
        }
        @endcode

        @see wxUpdateUIEvent, DoUpdateWindowUI(), OnInternalIdle()
    */
    virtual void UpdateWindowUI(long flags = wxUPDATE_UI_NONE);

    //@}


    // NOTE: static functions must have their own group or Doxygen will screw
    //       up the ordering of the member groups

    /**
        @name Miscellaneous static functions
    */
    //@{

    /**
        Returns the default font and colours which are used by the control.

        This is useful if you want to use the same font or colour in your own control
        as in a standard control -- which is a much better idea than hard coding specific
        colours or fonts which might look completely out of place on the users
        system, especially if it uses themes.

        The @a variant parameter is only relevant under Mac currently and is
        ignore under other platforms. Under Mac, it will change the size of the
        returned font. See SetWindowVariant() for more about this.

        This static method is "overridden" in many derived classes and so calling,
        for example, wxButton::GetClassDefaultAttributes() will typically
        return the values appropriate for a button which will be normally different
        from those returned by, say, wxListCtrl::GetClassDefaultAttributes().

        The @c wxVisualAttributes structure has at least the fields
        @c font, @c colFg and @c colBg. All of them may be invalid
        if it was not possible to determine the default control appearance or,
        especially for the background colour, if the field doesn't make sense as is
        the case for @c colBg for the controls with themed background.

        @see InheritAttributes()
    */
    static wxVisualAttributes GetClassDefaultAttributes(wxWindowVariant variant = wxWINDOW_VARIANT_NORMAL);

    /**
        Finds the window or control which currently has the keyboard focus.

        @remarks Note that this is a static function, so it can be called without
                 needing a wxWindow pointer.

        @see SetFocus(), HasFocus()
    */
    static wxWindow* FindFocus();

    /**
        Find the first window with the given @e id.

        If @a parent is @NULL, the search will start from all top-level frames
        and dialog boxes; if non-@NULL, the search will be limited to the given
        window hierarchy.
        The search is recursive in both cases.

        @see FindWindow()

        @return Window with the given @a id or @NULL if not found.
    */
    static wxWindow* FindWindowById(long id, const wxWindow* parent = 0);

    /**
        Find a window by its label.

        Depending on the type of window, the label may be a window title
        or panel item label. If @a parent is @NULL, the search will start from all
        top-level frames and dialog boxes; if non-@NULL, the search will be
        limited to the given window hierarchy.

        The search is recursive in both cases and, unlike with FindWindow(),
        recurses into top level child windows too.

        @see FindWindow()

        @return Window with the given @a label or @NULL if not found.
    */
    static wxWindow* FindWindowByLabel(const wxString& label,
                                       const wxWindow* parent = 0);

    /**
        Find a window by its name (as given in a window constructor or Create()
        function call).

        If @a parent is @NULL, the search will start from all top-level frames
        and dialog boxes; if non-@NULL, the search will be limited to the given
        window hierarchy.

        The search is recursive in both cases and, unlike FindWindow(),
        recurses into top level child windows too.

        If no window with such name is found, FindWindowByLabel() is called,
        i.e. the name is interpreted as (internal) name first but if this
        fails, it's internal as (user-visible) label. As this behaviour may be
        confusing, it is usually better to use either the FindWindow() overload
        taking the name or FindWindowByLabel() directly.

        @return Window with the given @a name or @NULL if not found.
    */
    static wxWindow* FindWindowByName(const wxString& name,
                                      const wxWindow* parent = 0);

    /**
        Returns the currently captured window.

        @see HasCapture(), CaptureMouse(), ReleaseMouse(),
             wxMouseCaptureLostEvent, wxMouseCaptureChangedEvent
    */
    static wxWindow* GetCapture();

    /**
        Create a new ID or range of IDs that are not currently in use.
        The IDs will be reserved until assigned to a wxWindow ID
        or unreserved with UnreserveControlId().

        See @ref overview_windowids for more information.

        @param count
            The number of sequential IDs to reserve.

        @return Returns the ID or the first ID of the range (i.e. the most negative),
                or wxID_NONE if the specified number of identifiers couldn't be allocated.

        @see UnreserveControlId(), wxIdManager,
             @ref overview_windowids
    */
    static wxWindowID NewControlId(int count = 1);

    /**
        Unreserve an ID or range of IDs that was reserved by NewControlId().
        See @ref overview_windowids for more information.

        @param id
            The starting ID of the range of IDs to unreserve.
        @param count
            The number of sequential IDs to unreserve.

        @see NewControlId(), wxIdManager, @ref overview_windowids
    */
    static void UnreserveControlId(wxWindowID id, int count = 1);

    //@}



protected:

    /**
        Centres the window.

        @param direction
            Specifies the direction for the centring. May be wxHORIZONTAL,
            wxVERTICAL or wxBOTH. It may also include the wxCENTRE_ON_SCREEN
            flag.

        @remarks This function is not meant to be called directly by user code,
                 but via Centre, Center, CentreOnParent, or CenterOnParent.
                 This function can be overridden to fine-tune centring behaviour.
    */
    virtual void DoCentre(int direction);

    /**
        Implementation of GetBestSize() that can be overridden.

        Notice that it is usually more convenient to override
        DoGetBestClientSize() rather than this method itself as you need to
        explicitly account for the window borders size if you do the latter.

        The default implementation of this function is designed for use in container
        windows, such as wxPanel, and works something like this:
        -# If the window has a sizer then it is used to calculate the best size.
        -# Otherwise if the window has layout constraints then those are used to
           calculate the best size.
        -# Otherwise if the window has children then the best size is set to be large
           enough to show all the children.
        -# Otherwise if there are no children then the window's minimal size will be
           used as its best size.
        -# Otherwise if there is no minimal size set, then the current size is used
           for the best size.

        @see @ref overview_windowsizing
    */
    virtual wxSize DoGetBestSize() const;

    /**
        Override this method to return the best size for a custom control.

        A typical implementation of this method should compute the minimal size
        needed to fully display the control contents taking into account the
        current font size.

        The default implementation simply returns ::wxDefaultSize and
        GetBestSize() returns an arbitrary hardcoded size for the window, so
        you must override it when implementing a custom window class.

        Notice that the best size returned by this function is cached
        internally, so if anything that results in the best size changing (e.g.
        change to the control contents) happens, you need to call
        InvalidateBestSize() to notify wxWidgets about it.

        @see @ref overview_windowsizing

        @since 2.9.0
     */
    virtual wxSize DoGetBestClientSize() const;

    /**
        Override this method to implement height-for-width best size
        calculation.

        Return the height needed to fully display the control contents if its
        width is fixed to the given value. Custom classes implementing
        wrapping should override this method and return the height
        corresponding to the number of lines needed to lay out the control
        contents at this width.

        Currently this method is not used by wxWidgets yet, however it is
        planned that it will be used by the new sizer classes implementing
        height-for-width layout strategy in the future.

        Notice that implementing this method or even implementing both it and
        DoGetBestClientWidth() doesn't replace overriding DoGetBestClientSize(),
        i.e. you still need to implement the latter as well in order to provide
        the best size when neither width nor height are constrained.

        By default returns ::wxDefaultCoord meaning that the vertical component
        of DoGetBestClientSize() return value should be used.

        @since 2.9.4
     */
    virtual int DoGetBestClientHeight(int width) const;

    /**
        Override this method to implement width-for-height best size
        calculation.

        This method is exactly the same as DoGetBestClientHeight() except that
        it determines the width assuming the height is fixed instead of vice
        versa.

        @since 2.9.4
     */
    virtual int DoGetBestClientWidth(int height) const;

    /**
        Sets the initial window size if none is given (i.e.\ at least one of the
        components of the size passed to ctor/Create() is wxDefaultCoord).
        @deprecated Use SetInitialSize() instead.
    */
    virtual void SetInitialBestSize(const wxSize& size);

    /**
        Generate wxWindowDestroyEvent for this window.

        This is called by the window itself when it is being destroyed and
        usually there is no need to call it but see wxWindowDestroyEvent for
        explanations of when you might want to do it.
     */
    void SendDestroyEvent();

    /**
        This function is public in wxEvtHandler but protected in wxWindow
        because for wxWindows you should always call ProcessEvent() on the
        pointer returned by GetEventHandler() and not on the wxWindow object
        itself.

        For convenience, a ProcessWindowEvent() method is provided as a synonym
        for @code GetEventHandler()->ProcessEvent() @endcode

        Note that it's still possible to call these functions directly on the
        wxWindow object (e.g. casting it to wxEvtHandler) but doing that will
        create subtle bugs when windows with event handlers pushed on them are
        involved.

        This holds also for all other wxEvtHandler functions.
    */
    virtual bool ProcessEvent(wxEvent& event);

    //@{
    /**
        See ProcessEvent() for more info about why you shouldn't use this function
        and the reason for making this function protected in wxWindow.
    */
    bool SafelyProcessEvent(wxEvent& event);
    virtual void QueueEvent(wxEvent *event);
    virtual void AddPendingEvent(const wxEvent& event);
    void ProcessPendingEvents();
    bool ProcessThreadEvent(const wxEvent& event);
    //@}
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_misc */
//@{

/**
    Find the deepest window at the mouse pointer position, returning the window
    and current pointer position in screen coordinates.

    @header{wx/window.h}
*/
wxWindow* wxFindWindowAtPointer(wxPoint& pt);

/**
    Gets the currently active window (implemented for MSW and GTK only
    currently, always returns @NULL in the other ports).

    @header{wx/window.h}
*/
wxWindow* wxGetActiveWindow();

/**
    Returns the first top level parent of the given window, or in other words,
    the frame or dialog containing it, or @NULL.

    Notice that if @a window is itself already a TLW, it is returned directly.

    @header{wx/window.h}
*/
wxWindow* wxGetTopLevelParent(wxWindow* window);

//@}

