/////////////////////////////////////////////////////////////////////////////
// Name:        scrolbar.h
// Purpose:     interface of wxScrollBar
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxScrollBar

    A wxScrollBar is a control that represents a horizontal or vertical scrollbar.

    It is distinct from the two scrollbars that some windows provide automatically,
    but the two types of scrollbar share the way events are received.

    @remarks
    A scrollbar has the following main attributes: range, thumb size, page size, and position.
    The range is the total number of units associated with the view represented by the scrollbar.
    For a table with 15 columns, the range would be 15.
    The thumb size is the number of units that are currently visible.
    For the table example, the window might be sized so that only 5 columns are
    currently visible, in which case the application would set the thumb size to 5.
    When the thumb size becomes the same as or greater than the range, the scrollbar
    will be automatically hidden on most platforms.
    The page size is the number of units that the scrollbar should scroll by,
    when 'paging' through the data. This value is normally the same as the thumb
    size length, because it is natural to assume that the visible window size defines a page.
    The scrollbar position is the current thumb position.
    Most applications will find it convenient to provide a function called AdjustScrollbars()
    which can be called initially, from an OnSize event handler, and whenever the
    application data changes in size. It will adjust the view, object and page
    size according to the size of the window and the size of the data.

    @beginStyleTable
    @style{wxSB_HORIZONTAL}
           Specifies a horizontal scrollbar.
    @style{wxSB_VERTICAL}
           Specifies a vertical scrollbar.
    @endStyleTable

    @beginEventEmissionTable{wxScrollEvent}
    You can use EVT_COMMAND_SCROLL... macros with window IDs for when intercepting
    scroll events from controls, or EVT_SCROLL... macros without window IDs for
    intercepting scroll events from the receiving window -- except for this,
    the macros behave exactly the same.
    @event{EVT_SCROLL(func)}
        Process all scroll events.
    @event{EVT_SCROLL_TOP(func)}
        Process @c wxEVT_SCROLL_TOP scroll to top or leftmost (minimum) position events.
    @event{EVT_SCROLL_BOTTOM(func)}
        Process @c wxEVT_SCROLL_BOTTOM scroll to bottom or rightmost (maximum) position events.
    @event{EVT_SCROLL_LINEUP(func)}
        Process @c wxEVT_SCROLL_LINEUP line up or left events.
    @event{EVT_SCROLL_LINEDOWN(func)}
        Process @c wxEVT_SCROLL_LINEDOWN line down or right events.
    @event{EVT_SCROLL_PAGEUP(func)}
        Process @c wxEVT_SCROLL_PAGEUP page up or left events.
    @event{EVT_SCROLL_PAGEDOWN(func)}
        Process @c wxEVT_SCROLL_PAGEDOWN page down or right events.
    @event{EVT_SCROLL_THUMBTRACK(func)}
        Process @c wxEVT_SCROLL_THUMBTRACK thumbtrack events
        (frequent events sent as the user drags the thumbtrack).
    @event{EVT_SCROLL_THUMBRELEASE(func)}
        Process @c wxEVT_SCROLL_THUMBRELEASE thumb release events.
    @event{EVT_SCROLL_CHANGED(func)}
        Process @c wxEVT_SCROLL_CHANGED end of scrolling events (MSW only).
    @event{EVT_COMMAND_SCROLL(id, func)}
        Process all scroll events.
    @event{EVT_COMMAND_SCROLL_TOP(id, func)}
        Process @c wxEVT_SCROLL_TOP scroll to top or leftmost (minimum) position events.
    @event{EVT_COMMAND_SCROLL_BOTTOM(id, func)}
        Process @c wxEVT_SCROLL_BOTTOM scroll to bottom or rightmost (maximum) position events.
    @event{EVT_COMMAND_SCROLL_LINEUP(id, func)}
        Process @c wxEVT_SCROLL_LINEUP line up or left events.
    @event{EVT_COMMAND_SCROLL_LINEDOWN(id, func)}
        Process @c wxEVT_SCROLL_LINEDOWN line down or right events.
    @event{EVT_COMMAND_SCROLL_PAGEUP(id, func)}
        Process @c wxEVT_SCROLL_PAGEUP page up or left events.
    @event{EVT_COMMAND_SCROLL_PAGEDOWN(id, func)}
        Process @c wxEVT_SCROLL_PAGEDOWN page down or right events.
    @event{EVT_COMMAND_SCROLL_THUMBTRACK(id, func)}
        Process @c wxEVT_SCROLL_THUMBTRACK thumbtrack events
        (frequent events sent as the user drags the thumbtrack).
    @event{EVT_COMMAND_SCROLL_THUMBRELEASE(func)}
        Process @c wxEVT_SCROLL_THUMBRELEASE thumb release events.
    @event{EVT_COMMAND_SCROLL_CHANGED(func)}
        Process @c wxEVT_SCROLL_CHANGED end of scrolling events (MSW only).
    @endEventTable

    @section scrollbar_diff The difference between EVT_SCROLL_THUMBRELEASE and EVT_SCROLL_CHANGED

    The EVT_SCROLL_THUMBRELEASE event is only emitted when actually dragging the
    thumb using the mouse and releasing it (This EVT_SCROLL_THUMBRELEASE event
    is also followed by an EVT_SCROLL_CHANGED event).

    The EVT_SCROLL_CHANGED event also occurs when using the keyboard to change
    the thumb position, and when clicking next to the thumb
    (In all these cases the EVT_SCROLL_THUMBRELEASE event does not happen).

    In short, the EVT_SCROLL_CHANGED event is triggered when scrolling/moving has
    finished independently of the way it had started. Please see the @ref page_samples_widgets
    ("Slider" page) to see the difference between EVT_SCROLL_THUMBRELEASE and
    EVT_SCROLL_CHANGED in action.

    @library{wxcore}
    @category{ctrl}
    @appearance{scrollbar}

    @see @ref overview_scrolling, @ref overview_events, wxScrolled
*/
class wxScrollBar : public wxControl
{
public:
    /**
      Default constructor
    */
    wxScrollBar();

    /**
        Constructor, creating and showing a scrollbar.

        @param parent
            Parent window. Must be non-@NULL.
        @param id
            Window identifier. The value wxID_ANY indicates a default value.
        @param pos
            Window position.
            If ::wxDefaultPosition is specified then a default position is chosen.
        @param size
            Window size.
            If ::wxDefaultSize is specified then a default size is chosen.
        @param style
            Window style. See wxScrollBar.
        @param validator
            Window validator.
        @param name
            Window name.

        @see Create(), wxValidator
    */
    wxScrollBar(wxWindow* parent, wxWindowID id,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxSB_HORIZONTAL,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxScrollBarNameStr);

    /**
        Destructor, destroying the scrollbar.
    */
    virtual ~wxScrollBar();

    /**
        Scrollbar creation function called by the scrollbar constructor.
        See wxScrollBar() for details.
    */
    bool Create(wxWindow* parent, wxWindowID id,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize, long style = wxSB_HORIZONTAL,
                const wxValidator& validator = wxDefaultValidator,
                const wxString& name = wxScrollBarNameStr);

    /**
        Returns the page size of the scrollbar.

        This is the number of scroll units that will be scrolled when the user
        pages up or down. Often it is the same as the thumb size.

        @see SetScrollbar()
    */
    virtual int GetPageSize() const;

    /**
        Returns the length of the scrollbar.

        @see SetScrollbar()
    */
    virtual int GetRange() const;

    /**
        Returns the current position of the scrollbar thumb.

        @see SetThumbPosition()
    */
    virtual int GetThumbPosition() const;

    /**
        Returns the thumb or 'view' size.

        @see SetScrollbar()
    */
    virtual int GetThumbSize() const;

    /**
        Sets the scrollbar properties.

        @param position
            The position of the scrollbar in scroll units.
        @param thumbSize
            The size of the thumb, or visible portion of the scrollbar, in scroll units.
        @param range
            The maximum position of the scrollbar.
        @param pageSize
            The size of the page size in scroll units. This is the number of units
            the scrollbar will scroll when it is paged up or down.
            Often it is the same as the thumb size.
        @param refresh
            @true to redraw the scrollbar, @false otherwise.

        @remarks
            Let's say you wish to display 50 lines of text, using the same
            font. The window is sized so that you can only see 16 lines at a time.
            You would use:
            @code
            scrollbar->SetScrollbar(0, 16, 50, 15);
            @endcode
            The page size is 1 less than the thumb size so that the last line of
            the previous page will be visible on the next page, to help orient the user.
            Note that with the window at this size, the thumb position can never
            go above 50 minus 16, or 34.
            You can determine how many lines are currently visible by dividing
            the current view size by the character height in pixels.
            When defining your own scrollbar behaviour, you will always need to
            recalculate the scrollbar settings when the window size changes.
            You could therefore put your scrollbar calculations and SetScrollbar()
            call into a function named AdjustScrollbars, which can be called
            initially and also from a wxSizeEvent event handler function.
    */
    virtual void SetScrollbar(int position, int thumbSize, int range,
                              int pageSize,
                              bool refresh = true);

    /**
        Sets the position of the scrollbar.

        @param viewStart
            The position of the scrollbar thumb.

        @see GetThumbPosition()
    */
    virtual void SetThumbPosition(int viewStart);

    /**
       Returns @true for scrollbars that have the vertical style set.
    */
    bool IsVertical() const;
};

