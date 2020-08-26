/////////////////////////////////////////////////////////////////////////////
// Name:        scrolwin.h
// Purpose:     interface of wxScrolled template
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Possible values for the second argument of wxScrolled::ShowScrollbars().
 */
enum wxScrollbarVisibility
{
    wxSHOW_SB_NEVER = -1,   ///< Never show the scrollbar at all.
    wxSHOW_SB_DEFAULT,      ///< Show scrollbar only if it is needed.
    wxSHOW_SB_ALWAYS        ///< Always show scrollbar, even if not needed.
};

/**

    The wxScrolled class manages scrolling for its client area, transforming
    the coordinates according to the scrollbar positions, and setting the
    scroll positions, thumb sizes and ranges according to the area in view.

    There are two commonly used (but not the only possible!) specializations of
    this class:

    - ::wxScrolledWindow, aka wxScrolled<wxPanel>, is equivalent to
      ::wxScrolledWindow from earlier versions. Derived from wxPanel, it shares
      wxPanel's behaviour with regard to TAB traversal and focus handling. Use
      this if the scrolled window will have child controls.

    - ::wxScrolledCanvas, aka wxScrolled<wxWindow>, derives from wxWindow and
      so doesn't handle children specially. This is suitable e.g. for
      implementing scrollable controls such as tree or list controls.

    @note
    See wxScrolled::Create() if you want to use wxScrolled with a custom class.

    Starting from version 2.4 of wxWidgets, there are several ways to use a
    ::wxScrolledWindow (and now wxScrolled). In particular, there are
    three ways to set the size of the scrolling area:

    One way is to set the scrollbars directly using a call to SetScrollbars().
    This is the way it used to be in any previous version of wxWidgets and it
    will be kept for backwards compatibility.

    An additional method of manual control, which requires a little less
    computation of your own, is to set the total size of the scrolling area by
    calling either wxWindow::SetVirtualSize(), or wxWindow::FitInside(), and
    setting the scrolling increments for it by calling SetScrollRate().
    Scrolling in some orientation is enabled by setting a non-zero increment
    for it.

    The most automatic and newest way is to simply let sizers determine the
    scrolling area. This is now the default when you set an interior sizer into
    a wxScrolled with wxWindow::SetSizer().  The scrolling area will be
    set to the size requested by the sizer and the scrollbars will be assigned
    for each orientation according to the need for them and the scrolling
    increment set by SetScrollRate().  As above, scrolling is only enabled in
    orientations with a non-zero increment.  You can influence the minimum size
    of the scrolled area controlled by a sizer by calling
    wxWindow::SetVirtualSizeHints(). (Calling SetScrollbars() has analogous
    effects in wxWidgets 2.4 -- in later versions it may not continue to
    override the sizer.)

    Note that if maximum size hints are still supported by
    wxWindow::SetVirtualSizeHints(), use them at your own dire risk. They may
    or may not have been removed for 2.4, but it really only makes sense to set
    minimum size hints here.  We should probably replace
    wxWindow::SetVirtualSizeHints() with wxWindow::SetMinVirtualSize() or
    similar and remove it entirely in future.

    @todo review docs for this class replacing SetVirtualSizeHints() with
          SetMinClientSize().

    As with all windows, an application can draw onto a wxScrolled using a
    @ref overview_dc "device context".

    You have the option of handling the OnPaint handler or overriding the
    wxScrolled::OnDraw() function, which is passed a pre-scrolled device
    context (prepared by wxScrolled::DoPrepareDC()).

    If you don't wish to calculate your own scrolling, you must call
    DoPrepareDC() when not drawing from within OnDraw(), to set the device
    origin for the device context according to the current scroll position.

    A wxScrolled will normally scroll itself and therefore its child windows
    as well. It might however be desired to scroll a different window than
    itself: e.g. when designing a spreadsheet, you will normally only have to
    scroll the (usually white) cell area, whereas the (usually grey) label area
    will scroll very differently. For this special purpose, you can call
    SetTargetWindow() which means that pressing the scrollbars will scroll a
    different window.

    Note that the underlying system knows nothing about scrolling coordinates,
    so that all system functions (mouse events, expose events, refresh calls
    etc) as well as the position of subwindows are relative to the "physical"
    origin of the scrolled window. If the user insert a child window at
    position (10,10) and scrolls the window down 100 pixels (moving the child
    window out of the visible area), the child window will report a position
    of (10,-90).

    @beginStyleTable
    @style{wxHSCROLL}
           If this style is specified and ::wxVSCROLL isn't, the window will be
           scrollable only in horizontal direction (by default, i.e. if neither
           this style nor ::wxVSCROLL is specified, it scrolls in both
           directions).
    @style{wxVSCROLL}
           If this style is specified and ::wxHSCROLL isn't, the window will be
           scrollable only in vertical direction (by default, i.e. if neither
           this style nor ::wxHSCROLL is specified, it scrolls in both
           directions).
    @style{wxALWAYS_SHOW_SB}
           Since wxWidgets 2.9.5, specifying this style makes the window always
           show its scrollbars, even if they are not used. See ShowScrollbars().
    @style{wxRETAINED}
           Uses a backing pixmap to speed refreshes. Motif only.
    @endStyleTable


    @beginEventEmissionTable{wxScrollWinEvent}
    @event{EVT_SCROLLWIN(func)}
        Process all scroll events.
    @event{EVT_SCROLLWIN_TOP(func)}
        Process @c wxEVT_SCROLLWIN_TOP scroll-to-top events.
    @event{EVT_SCROLLWIN_BOTTOM(func)}
        Process @c wxEVT_SCROLLWIN_BOTTOM scroll-to-bottom events.
    @event{EVT_SCROLLWIN_LINEUP(func)}
        Process @c wxEVT_SCROLLWIN_LINEUP line up events.
    @event{EVT_SCROLLWIN_LINEDOWN(func)}
        Process @c wxEVT_SCROLLWIN_LINEDOWN line down events.
    @event{EVT_SCROLLWIN_PAGEUP(func)}
        Process @c wxEVT_SCROLLWIN_PAGEUP page up events.
    @event{EVT_SCROLLWIN_PAGEDOWN(func)}
        Process @c wxEVT_SCROLLWIN_PAGEDOWN page down events.
    @event{EVT_SCROLLWIN_THUMBTRACK(func)}
        Process @c wxEVT_SCROLLWIN_THUMBTRACK thumbtrack events
        (frequent events sent as the user drags the thumbtrack).
    @event{EVT_SCROLLWIN_THUMBRELEASE(func)}
        Process @c wxEVT_SCROLLWIN_THUMBRELEASE thumb release events.
    @endEventTable

    @note
        Don't confuse wxScrollWinEvents generated by this class with
        wxScrollEvent objects generated by wxScrollBar and wxSlider.


    @remarks
    Use wxScrolled for applications where the user scrolls by a fixed amount,
    and where a 'page' can be interpreted to be the current visible portion of
    the window. For more sophisticated applications, use the wxScrolled
    implementation as a guide to build your own scroll behaviour or use
    wxVScrolledWindow or its variants.

    @since The wxScrolled template exists since version 2.9.0. In older versions,
           only ::wxScrolledWindow (equivalent of wxScrolled<wxPanel>) was
           available.

    @library{wxcore}
    @category{miscwnd}

    @see wxScrollBar, wxClientDC, wxPaintDC,
         wxVScrolledWindow, wxHScrolledWindow, wxHVScrolledWindow,
*/
template<class T>
class wxScrolled : public T
{
public:
    /// Default constructor.
    wxScrolled();

    /**
        Constructor.

        @param parent
            Parent window.
        @param id
            Window identifier. The value @c wxID_ANY indicates a default value.
        @param pos
            Window position. If a position of ::wxDefaultPosition is specified
            then a default position is chosen.
        @param size
            Window size. If a size of ::wxDefaultSize is specified then the
            window is sized appropriately.
        @param style
            Window style. See wxScrolled.
        @param name
            Window name.

        @remarks The window is initially created without visible scrollbars.
                 Call SetScrollbars() to specify how big the virtual window
                 size should be.
    */
    wxScrolled(wxWindow* parent, wxWindowID id = -1,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               long style = wxHSCROLL | wxVSCROLL,
               const wxString& name = "scrolledWindow");

    /**
        Translates the logical coordinates to the device ones. For example, if
        a window is scrolled 10 pixels to the bottom, the device coordinates of
        the origin are (0, 0) (as always), but the logical coordinates are (0,
        10) and so the call to CalcScrolledPosition(0, 10, xx, yy) will return
        0 in yy.

        @beginWxPerlOnly
        In wxPerl this method takes two parameters and returns a
        2-element list (xx, yy).
        @endWxPerlOnly

        @see CalcUnscrolledPosition()
    */
    void CalcScrolledPosition(int x, int y, int* xx, int* yy) const;
    wxPoint CalcScrolledPosition(const wxPoint& pt) const;

    /**
        Translates the device coordinates to the logical ones. For example, if
        a window is scrolled 10 pixels to the bottom, the device coordinates of
        the origin are (0, 0) (as always), but the logical coordinates are (0,
        10) and so the call to CalcUnscrolledPosition(0, 0, xx, yy) will return
        10 in yy.

        @beginWxPerlOnly
        In wxPerl this method takes two parameters and returns a
        2-element list (xx, yy).
        @endWxPerlOnly

        @see CalcScrolledPosition()
    */
    void CalcUnscrolledPosition(int x, int y, int* xx, int* yy) const;
    wxPoint CalcUnscrolledPosition(const wxPoint& pt) const;

    /**
        Creates the window for two-step construction. Derived classes
        should call or replace this function. If it is not replaced,
        bear in mind that it calls T::Create() through the global function
        wxCreateScrolled() so if T::Create() has a different signature
        than wxScrolled::Create() you should implement overloaded
        wxCreateScrolled() which would call T::Create() in the correct manner.

        @see wxScrolled::wxScrolled() and wxCreateScrolled() for details.
    */
    bool Create(wxWindow* parent, wxWindowID id = -1,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxHSCROLL | wxVSCROLL,
                const wxString& name = "scrolledWindow");

    /**
        Disable use of keyboard keys for scrolling.

        By default cursor movement keys (including Home, End, Page Up and Down)
        are used to scroll the window appropriately. If the derived class uses
        these keys for something else, e.g. changing the currently selected
        item, this function can be used to disable this behaviour as it's not
        only not necessary then but can actually be actively harmful if another
        object forwards a keyboard event corresponding to one of the above keys
        to us using ProcessWindowEvent() because the event will always be
        processed which can be undesirable.

        @since 2.9.1
    */
    void DisableKeyboardScrolling();

    /**
        Call this function to prepare the device context for drawing a scrolled
        image.

        It sets the device origin according to the current scroll position.
        DoPrepareDC() is called automatically within the default @c wxEVT_PAINT
        event handler, so your OnDraw() override will be passed an already
        'pre-scrolled' device context. However, if you wish to draw from
        outside of OnDraw() (e.g. from your own @c wxEVT_PAINT handler), you
        must call this function yourself.

        For example:
        @code
        void MyWindow::OnEvent(wxMouseEvent& event)
        {
          wxClientDC dc(this);
          DoPrepareDC(dc);

          dc.SetPen(*wxBLACK_PEN);
          float x, y;
          event.Position(&x, &y);
          if (xpos > -1 && ypos > -1 && event.Dragging())
          {
            dc.DrawLine(xpos, ypos, x, y);
          }
          xpos = x;
          ypos = y;
        }
        @endcode

        Notice that the function sets the origin by moving it relatively to the
        current origin position, so you shouldn't change the origin before
        calling DoPrepareDC() or, if you do, reset it to (0, 0) later. If you
        call DoPrepareDC() immediately after device context creation, as in the
        example above, this problem doesn't arise, of course, so it is
        customary to do it like this.
    */
    void DoPrepareDC(wxDC& dc);

    /**
        Enable or disable use of wxWindow::ScrollWindow() for scrolling.

        By default, when a scrolled window is logically scrolled,
        wxWindow::ScrollWindow() is called on the underlying window which
        scrolls the window contents and only invalidates the part of the window
        newly brought into view. If @false is passed as an argument, then this
        "physical scrolling" is disabled and the window is entirely invalidated
        whenever it is scrolled by calling wxWindow::Refresh().

        It should be rarely necessary to disable physical scrolling, so this
        method shouldn't be called in normal circumstances.

        @param xScrolling
            If @true, enables physical scrolling in the x direction.
        @param yScrolling
            If @true, enables physical scrolling in the y direction.
    */
    void EnableScrolling(bool xScrolling, bool yScrolling);

    /**
        Set the scrollbar visibility.

        By default the scrollbar in the corresponding direction is only shown
        if it is needed, i.e. if the virtual size of the scrolled window in
        this direction is greater than the current physical window size. Using
        this function the scrollbar visibility can be changed to be:
            - wxSHOW_SB_ALWAYS: To always show the scrollbar, even if it is
                not needed currently (wxALWAYS_SHOW_SB style can be used during
                the window creation to achieve the same effect but it applies
                in both directions).
            - wxSHOW_SB_NEVER: To never show the scrollbar at all. In this case
                the program should presumably provide some other way for the
                user to scroll the window.
            - wxSHOW_SB_DEFAULT: To restore the default behaviour described
                above.

        Note that the window must be created before calling this method.

        @param horz
            The desired visibility for the horizontal scrollbar.
        @param vert
            The desired visibility for the vertical scrollbar.

        @since 2.9.0
     */
    void ShowScrollbars(wxScrollbarVisibility horz, wxScrollbarVisibility vert);

    /**
        Get the number of pixels per scroll unit (line), in each direction, as
        set by SetScrollbars(). A value of zero indicates no scrolling in that
        direction.

        @param xUnit
            Receives the number of pixels per horizontal unit.
        @param yUnit
            Receives the number of pixels per vertical unit.

        @beginWxPerlOnly
        In wxPerl this method takes no parameters and returns a
        2-element list (xUnit, yUnit).
        @endWxPerlOnly

        @see SetScrollbars(), GetVirtualSize()
    */
    void GetScrollPixelsPerUnit(int* xUnit, int* yUnit) const;

    /**
        Get the position at which the visible portion of the window starts.

        @param x
            Receives the first visible x position in scroll units.
        @param y
            Receives the first visible y position in scroll units.

        @remarks
            If either of the scrollbars is not at the home position, @a x
            and/or @a y will be greater than zero.
            Combined with wxWindow::GetClientSize(), the application can use this
            function to efficiently redraw only the visible portion of the window.
            The positions are in logical scroll units, not pixels, so to convert
            to pixels you will have to multiply by the number of pixels per scroll
            increment.

        @beginWxPerlOnly
        In wxPerl this method takes no parameters and returns a
        2-element list (x, y).
        @endWxPerlOnly

        @see SetScrollbars(), Scroll()
    */
    void GetViewStart(int* x, int* y) const;

    /**
        This is a simple overload of GetViewStart(int*,int*); see that function
        for more info.
    */
    wxPoint GetViewStart() const;

    /**
        Gets the size in device units of the scrollable window area (as
        opposed to the client size, which is the area of the window currently
        visible).

        @param x
            Receives the length of the scrollable window, in pixels.
        @param y
            Receives the height of the scrollable window, in pixels.

        @remarks Use wxDC::DeviceToLogicalX() and wxDC::DeviceToLogicalY() to
                 translate these units to logical units.

        @beginWxPerlOnly
        In wxPerl this method takes no parameters and returns a
        2-element list (xUnit, yUnit).
        @endWxPerlOnly

        @see SetScrollbars(), GetScrollPixelsPerUnit()
    */
    void GetVirtualSize(int* x, int* y) const;

    /**
        Motif only: @true if the window has a backing bitmap.
    */
    bool IsRetained() const;

    /**
        Called by the default paint event handler to allow the application to
        define painting behaviour without having to worry about calling
        DoPrepareDC().

        Instead of overriding this function you may also just process the paint
        event in the derived class as usual, but then you will have to call
        DoPrepareDC() yourself.
    */
    virtual void OnDraw(wxDC& dc);

    /**
        This function is for backwards compatibility only and simply calls
        DoPrepareDC() now. Notice that it is not called by the default paint
        event handle (DoPrepareDC() is), so overriding this method in your
        derived class is useless.
    */
    void PrepareDC(wxDC& dc);

    /**
        Scrolls a window so the view start is at the given point.

        @param x
            The x position to scroll to, in scroll units.
        @param y
            The y position to scroll to, in scroll units.

        @remarks The positions are in scroll units, not pixels, so to convert to
                 pixels you will have to multiply by the number of
                 pixels per scroll increment. If either parameter is
                 ::wxDefaultCoord (-1), that position will be ignored (no change
                 in that direction).

        @see SetScrollbars(), GetScrollPixelsPerUnit()
    */
    void Scroll(int x, int y);

    /**
        This is an overload of Scroll(int,int); see that function for more info.
    */
    void Scroll(const wxPoint& pt);

    /**
        Set the horizontal and vertical scrolling increment only. See the
        pixelsPerUnit parameter in SetScrollbars().
    */
    void SetScrollRate(int xstep, int ystep);

    /**
        Sets up vertical and/or horizontal scrollbars.

        The first pair of parameters give the number of pixels per 'scroll
        step', i.e. amount moved when the up or down scroll arrows are pressed.
        The second pair gives the length of scrollbar in scroll steps, which
        sets the size of the virtual window.

        @a xPos and @a yPos optionally specify a position to scroll to
        immediately.

        For example, the following gives a window horizontal and vertical
        scrollbars with 20 pixels per scroll step, and a size of 50 steps (1000
        pixels) in each direction:
        @code
        window->SetScrollbars(20, 20, 50, 50);
        @endcode

        wxScrolled manages the page size itself, using the current client
        window size as the page size.

        Note that for more sophisticated scrolling applications, for example
        where scroll steps may be variable according to the position in the
        document, it will be necessary to derive a new class from wxWindow,
        overriding OnSize() and adjusting the scrollbars appropriately.

        @param pixelsPerUnitX
            Pixels per scroll unit in the horizontal direction.
        @param pixelsPerUnitY
            Pixels per scroll unit in the vertical direction.
        @param noUnitsX
            Number of units in the horizontal direction.
        @param noUnitsY
            Number of units in the vertical direction.
        @param xPos
            Position to initialize the scrollbars in the horizontal direction,
            in scroll units.
        @param yPos
            Position to initialize the scrollbars in the vertical direction, in
            scroll units.
        @param noRefresh
            Will not refresh window if @true.

        @see wxWindow::SetVirtualSize()
    */
    void SetScrollbars(int pixelsPerUnitX, int pixelsPerUnitY,
                       int noUnitsX,
                       int noUnitsY,
                       int xPos = 0,
                       int yPos = 0,
                       bool noRefresh = false);

    /**
        Call this function to tell wxScrolled to perform the actual scrolling
        on a different window (and not on itself).

        This method is useful when only a part of the window should be
        scrolled. A typical example is a control consisting of a fixed header
        and the scrollable contents window: the scrollbars are attached to the
        main window itself, hence it, and not the contents window must be
        derived from wxScrolled, but only the contents window scrolls when the
        scrollbars are used. To implement such setup, you need to call this
        method with the contents window as argument.

        Notice that if this method is used, GetSizeAvailableForScrollTarget()
        method must be overridden.
    */
    void SetTargetWindow(wxWindow *window);
    wxWindow *GetTargetWindow() const;


    void SetTargetRect(const wxRect& rect);
    wxRect GetTargetRect() const;

    int GetScrollPageSize(int orient) const;
    void SetScrollPageSize(int orient, int pageSize);
    int GetScrollLines( int orient ) const;
    void SetScale(double xs, double ys);
    double GetScaleX() const;
    double GetScaleY() const;

    virtual void AdjustScrollbars();

    /**
       Are we generating the autoscroll events?
     */
    bool IsAutoScrolling() const;

    /**
       Stop generating the scroll events when mouse is held outside the
       window.
     */
    void StopAutoScrolling();

    /**
       This method can be overridden in a derived class to forbid sending the
       auto scroll events - note that unlike StopAutoScrolling() it doesn't
       stop the timer, so it will be called repeatedly and will typically
       return different values depending on the current mouse position

       The base class version just returns true.
    */
    virtual bool SendAutoScrollEvents(wxScrollWinEvent& event) const;

protected:
    /**
        This method can be overridden in a derived class to prevent scrolling
        the child window into view automatically when it gets focus.

        The default behaviour is to scroll this window to show its currently
        focused child automatically, to ensure that the user can interact with
        it. This is usually helpful, but can be undesirable for some windows,
        in which case this method can be overridden to return @false for them
        to prevent any scrolling from taking place when such windows get focus.

        @since 3.1.3
     */
    virtual bool ShouldScrollToChildOnFocus(wxWindow* child);

    /**
        Function which must be overridden to implement the size available for
        the scroll target for the given size of the main window.

        This method must be overridden if SetTargetWindow() is used (it is
        never called otherwise). The implementation should decrease the @a size
        to account for the size of the non-scrollable parts of the main window
        and return only the size available for the scrollable window itself.
        E.g. in the example given in SetTargetWindow() documentation the
        function would subtract the height of the header window from the
        vertical component of @a size.
     */
    virtual wxSize GetSizeAvailableForScrollTarget(const wxSize& size);
};

/**
    Helper function which is called from wxScrolled::Create() to actually create
    a scrolled window. By default it just passes the call to the base class Create():
    @code
    self->Create(parent, winid, pos, size, style, name);
    @endcode

    You should provide overloaded implementation of this function for the custom
    base class if this class is created in a different manner, like it is e.g.
    done for wxControl:
    @code
    bool wxCreateScrolled(wxControl* self,
                          wxWindow *parent, wxWindowID winid,
                          const wxPoint& pos, const wxSize& size,
                          long style, const wxString& name)
    {
         return self->Create(parent, winid, pos, size, style, wxDefaultValidator, name);
    }
    @endcode

    @since 3.1.3
*/
bool wxCreateScrolled(T* self,
                      wxWindow *parent, wxWindowID winid,
                      const wxPoint& pos, const wxSize& size,
                      long style, const wxString& name);

/**
    Scrolled window derived from wxPanel.

    See wxScrolled for a detailed description.

    @note Note that because this class derives from wxPanel, it shares its
          behaviour with regard to TAB traversal and focus handling (in
          particular, it forwards focus to its children). If you don't want
          this behaviour, use ::wxScrolledCanvas instead.

    @note ::wxScrolledWindow is an alias for wxScrolled<wxPanel> since version
          2.9.0. In older versions, it was a standalone class.

    @library{wxcore}
    @category{miscwnd}

    @see wxScrolled, ::wxScrolledCanvas
*/
typedef wxScrolled<wxPanel> wxScrolledWindow;

/**
    Alias for wxScrolled<wxWindow>. Scrolled window that doesn't have children
    and so doesn't need or want special handling of TAB traversal.

    @since 2.9.0

    @library{wxcore}
    @category{miscwnd}

    @see wxScrolled, ::wxScrolledWindow
*/
typedef wxScrolled<wxWindow> wxScrolledCanvas;
