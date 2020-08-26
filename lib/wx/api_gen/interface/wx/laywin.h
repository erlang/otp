/////////////////////////////////////////////////////////////////////////////
// Name:        laywin.h
// Purpose:     interface of wxLayoutAlgorithm
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Enumeration used by wxLayoutAlgorithm.
*/
enum wxLayoutOrientation
{
    wxLAYOUT_HORIZONTAL,
    wxLAYOUT_VERTICAL
};

/**
    Enumeration used by wxLayoutAlgorithm.
*/
enum wxLayoutAlignment
{
    wxLAYOUT_NONE,
    wxLAYOUT_TOP,
    wxLAYOUT_LEFT,
    wxLAYOUT_RIGHT,
    wxLAYOUT_BOTTOM
};

/**
    @class wxLayoutAlgorithm

    wxLayoutAlgorithm implements layout of subwindows in MDI or SDI frames.
    It sends a wxCalculateLayoutEvent event to children of the frame, asking them
    for information about their size. For MDI parent frames, the algorithm allocates
    the remaining space to the MDI client window (which contains the MDI child frames).

    For SDI (normal) frames, a 'main' window is specified as taking up the
    remaining space.

    Because the event system is used, this technique can be applied to any windows,
    which are not necessarily 'aware' of the layout classes (no virtual functions
    in wxWindow refer to wxLayoutAlgorithm or its events).
    However, you may wish to use wxSashLayoutWindow for your subwindows since this
    class provides handlers for the required events, and accessors to specify the
    desired size of the window. The sash behaviour in the base class can be used,
    optionally, to make the windows user-resizable.

    wxLayoutAlgorithm is typically used in IDE (integrated development environment)
    applications, where there are several resizable windows in addition to the MDI
    client window, or other primary editing window. Resizable windows might include
    toolbars, a project window, and a window for displaying error and warning messages.

    When a window receives an OnCalculateLayout event, it should call SetRect in
    the given event object, to be the old supplied rectangle minus whatever space
    the window takes up. It should also set its own size accordingly.
    wxSashLayoutWindow::OnCalculateLayout generates an OnQueryLayoutInfo event
    which it sends to itself to determine the orientation, alignment and size of
    the window, which it gets from internal member variables set by the application.

    The algorithm works by starting off with a rectangle equal to the whole frame
    client area. It iterates through the frame children, generating
    wxLayoutAlgorithm::OnCalculateLayout events which subtract the window size and
    return the remaining rectangle for the next window to process.
    It is assumed (by wxSashLayoutWindow::OnCalculateLayout) that a window stretches
    the full dimension of the frame client, according to the orientation it specifies.
    For example, a horizontal window will stretch the full width of the remaining
    portion of the frame client area.
    In the other orientation, the window will be fixed to whatever size was
    specified by wxLayoutAlgorithm::OnQueryLayoutInfo. An alignment setting will
    make the window 'stick' to the left, top, right or bottom of the remaining
    client area. This scheme implies that order of window creation is important.
    Say you wish to have an extra toolbar at the top of the frame, a project window
    to the left of the MDI client window, and an output window above the status bar.
    You should therefore create the windows in this order: toolbar, output window,
    project window. This ensures that the toolbar and output window take up space
    at the top and bottom, and then the remaining height in-between is used for
    the project window.

    wxLayoutAlgorithm is quite independent of the way in which
    wxLayoutAlgorithm::OnCalculateLayout chooses to interpret a window's size and
    alignment. Therefore you could implement a different window class with a new
    wxLayoutAlgorithm::OnCalculateLayout event handler, that has a more sophisticated
    way of laying out the windows. It might allow specification of whether stretching
    occurs in the specified orientation, for example, rather than always assuming
    stretching.
    (This could, and probably should, be added to the existing implementation).

    @note wxLayoutAlgorithm has nothing to do with wxLayoutConstraints.
          It is an alternative way of specifying layouts for which the normal
          constraint system is unsuitable.

    @beginEventEmissionTable{wxQueryLayoutInfoEvent,wxCalculateLayoutEvent}
    @event{EVT_QUERY_LAYOUT_INFO(func)}
        Process a @c wxEVT_QUERY_LAYOUT_INFO event, to get size, orientation and
        alignment from a window. See wxQueryLayoutInfoEvent.
    @event{EVT_CALCULATE_LAYOUT(func)}
        Process a @c wxEVT_CALCULATE_LAYOUT event, which asks the window to take a
        'bite' out of a rectangle provided by the algorithm. See wxCalculateLayoutEvent.
    @endEventTable

    Note that the algorithm object does not respond to events, but itself generates the
    previous events in order to calculate window sizes.


    @library{wxcore}
    @category{winlayout}

    @see wxSashEvent, wxSashLayoutWindow, @ref overview_events
*/
class wxLayoutAlgorithm : public wxObject
{
public:
    /**
        Default constructor.
    */
    wxLayoutAlgorithm();

    /**
        Destructor.
    */
    virtual ~wxLayoutAlgorithm();

    /**
        Lays out the children of a normal frame. @a mainWindow is set to occupy the
        remaining space. This function simply calls LayoutWindow().
    */
    bool LayoutFrame(wxFrame* frame, wxWindow* mainWindow = NULL);

    /**
        Lays out the children of an MDI parent frame. If @a rect is non-@NULL, the
        given rectangle will be used as a starting point instead of the frame's client
        area. The MDI client window is set to occupy the remaining space.
    */
    bool LayoutMDIFrame(wxMDIParentFrame* frame, wxRect* rect = NULL);

    /**
        Lays out the children of a normal frame or other window.

        @a mainWindow is set to occupy the remaining space. If this is not specified,
        then the last window that responds to a calculate layout event in query mode will
        get the remaining space (that is, a non-query OnCalculateLayout event will
        not be sent to this window and the window will be set to the remaining size).
    */
    bool LayoutWindow(wxWindow* parent, wxWindow* mainWindow = NULL);
};



/**
    @class wxSashLayoutWindow

    wxSashLayoutWindow responds to OnCalculateLayout events generated by wxLayoutAlgorithm.
    It allows the  application to use simple accessors to specify how the window
    should be laid out, rather than having to respond to events.

    The fact that the class derives from wxSashWindow allows sashes to be used if
    required, to allow the windows to be user-resizable.

    The documentation for wxLayoutAlgorithm explains the purpose of this class in
    more detail.

    For the window styles see wxSashWindow.

    This class handles the EVT_QUERY_LAYOUT_INFO and EVT_CALCULATE_LAYOUT events
    for you. However, if you use sashes, see wxSashWindow for relevant event information.
    See also wxLayoutAlgorithm for information about the layout events.

    @library{wxcore}
    @category{miscwnd}

    @see wxLayoutAlgorithm, wxSashWindow, @ref overview_events
*/
class wxSashLayoutWindow : public wxSashWindow
{
public:
    /**
        Default ctor.
    */
    wxSashLayoutWindow();

    /**
        Constructs a sash layout window, which can be a child of a frame, dialog or any
        other non-control window.

        @param parent
            Pointer to a parent window.
        @param id
            Window identifier. If -1, will automatically create an identifier.
        @param pos
            Window position. wxDefaultPosition is (-1, -1) which indicates that
            wxSashLayoutWindows should generate a default position for the window.
            If using the wxSashLayoutWindow class directly, supply an actual position.
        @param size
            Window size. wxDefaultSize is (-1, -1) which indicates that
            wxSashLayoutWindows should generate a default size for the window.
        @param style
            Window style. For window styles, please see wxSashLayoutWindow.
        @param name
            Window name.
    */
    wxSashLayoutWindow(wxWindow* parent, wxWindowID id,
                       const wxPoint& pos = wxDefaultPosition,
                       const wxSize& size = wxDefaultSize,
                       long style = wxCLIP_CHILDREN | wxSW_3D,
                       const wxString& name = "layoutWindow");

    /**
        Initializes a sash layout window, which can be a child of a frame, dialog or
        any other non-control window.

        @param parent
            Pointer to a parent window.
        @param id
            Window identifier. If -1, will automatically create an identifier.
        @param pos
            Window position. wxDefaultPosition is (-1, -1) which indicates that
            wxSashLayoutWindows should generate a default position for the window.
            If using the wxSashLayoutWindow class directly, supply an actual position.
        @param size
            Window size. wxDefaultSize is (-1, -1) which indicates that
            wxSashLayoutWindows should generate a default size for the window.
        @param style
            Window style. For window styles, please see wxSashLayoutWindow.
        @param name
            Window name.
    */
    bool Create(wxWindow* parent, wxWindowID id = wxID_ANY,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxCLIP_CHILDREN | wxSW_3D,
                const wxString& name = "layoutWindow");

    /**
        Returns the alignment of the window: one of wxLAYOUT_TOP, wxLAYOUT_LEFT,
        wxLAYOUT_RIGHT, wxLAYOUT_BOTTOM.
    */
    wxLayoutAlignment GetAlignment() const;

    /**
        Returns the orientation of the window: one of wxLAYOUT_HORIZONTAL,
        wxLAYOUT_VERTICAL.
    */
    wxLayoutOrientation GetOrientation() const;

    /**
        The default handler for the event that is generated by wxLayoutAlgorithm.
        The implementation of this function calls wxCalculateLayoutEvent::SetRect
        to shrink the provided size according to how much space this window takes up.
        For further details, see wxLayoutAlgorithm and wxCalculateLayoutEvent.
    */
    void OnCalculateLayout(wxCalculateLayoutEvent& event);

    /**
        The default handler for the event that is generated by OnCalculateLayout to get
        size, alignment and orientation information for the window.
        The implementation of this function uses member variables as set by accessors
        called by the application.

        For further details, see wxLayoutAlgorithm and wxQueryLayoutInfoEvent.
    */
    void OnQueryLayoutInfo(wxQueryLayoutInfoEvent& event);

    /**
        Sets the alignment of the window (which edge of the available parent client
        area the window is attached to). @a alignment is one of wxLAYOUT_TOP, wxLAYOUT_LEFT,
        wxLAYOUT_RIGHT, wxLAYOUT_BOTTOM.
    */
    void SetAlignment(wxLayoutAlignment alignment);

    /**
        Sets the default dimensions of the window. The dimension other than the
        orientation will be fixed to this value, and the orientation dimension
        will be ignored and the window stretched to fit the available space.
    */
    void SetDefaultSize(const wxSize& size);

    /**
        Sets the orientation of the window (the direction the window will stretch in,
        to fill the available parent client area). @a orientation is one of
        wxLAYOUT_HORIZONTAL, wxLAYOUT_VERTICAL.
    */
    void SetOrientation(wxLayoutOrientation orientation);
};



/**
    @class wxQueryLayoutInfoEvent

    This event is sent when wxLayoutAlgorithm wishes to get the size, orientation
    and alignment of a window. More precisely, the event is sent by the
    OnCalculateLayout handler which is itself invoked by wxLayoutAlgorithm.

    @beginEventTable{wxQueryLayoutInfoEvent}
    @event{EVT_QUERY_LAYOUT_INFO(func)}
        Process a @c wxEVT_QUERY_LAYOUT_INFO event, to get size, orientation and alignment
        from a window.
    @endEventTable

    @library{wxcore}
    @category{events}

    @see wxCalculateLayoutEvent, wxSashLayoutWindow, wxLayoutAlgorithm.
*/
class wxQueryLayoutInfoEvent : public wxEvent
{
public:
    /**
        Constructor.
    */
    wxQueryLayoutInfoEvent(wxWindowID id = 0);

    /**
        Specifies the alignment of the window (which side of the remaining parent
        client area the window sticks to).
        One of wxLAYOUT_TOP, wxLAYOUT_LEFT, wxLAYOUT_RIGHT, wxLAYOUT_BOTTOM.
    */
    wxLayoutAlignment GetAlignment() const;

    /**
        Returns the flags associated with this event. Not currently used.
    */
    int GetFlags() const;

    /**
        Returns the orientation that the event handler specified to the event object.
        May be one of wxLAYOUT_HORIZONTAL, wxLAYOUT_VERTICAL.
    */
    wxLayoutOrientation GetOrientation() const;

    /**
        Returns the requested length of the window in the direction of the window
        orientation. This information is not yet used.
    */
    int GetRequestedLength() const;

    /**
        Returns the size that the event handler specified to the event object as being
        the requested size of the window.
    */
    wxSize GetSize() const;

    /**
        Call this to specify the alignment of the window (which side of the remaining
        parent client area the window sticks to).
        May be one of wxLAYOUT_TOP, wxLAYOUT_LEFT, wxLAYOUT_RIGHT, wxLAYOUT_BOTTOM.
    */
    void SetAlignment(wxLayoutAlignment alignment);

    /**
        Sets the flags associated with this event. Not currently used.
    */
    void SetFlags(int flags);

    /**
        Call this to specify the orientation of the window.
        May be one of wxLAYOUT_HORIZONTAL, wxLAYOUT_VERTICAL.
    */
    void SetOrientation(wxLayoutOrientation orientation);

    /**
        Sets the requested length of the window in the direction of the window
        orientation. This information is not yet used.
    */
    void SetRequestedLength(int length);

    /**
        Call this to let the calling code know what the size of the window is.
    */
    void SetSize(const wxSize& size);
};


wxEventType wxEVT_QUERY_LAYOUT_INFO;


/**
    @class wxCalculateLayoutEvent

    This event is sent by wxLayoutAlgorithm to calculate the amount of the
    remaining client area that the window should occupy.

    @beginEventTable{wxCalculateLayoutEvent}
    @event{EVT_CALCULATE_LAYOUT(func)}
        Process a @c wxEVT_CALCULATE_LAYOUT event, which asks the window to take a
        'bite' out of a rectangle provided by the algorithm.
    @endEventTable

    @library{wxcore}
    @category{events}

    @see wxQueryLayoutInfoEvent, wxSashLayoutWindow, wxLayoutAlgorithm.
*/
class wxCalculateLayoutEvent : public wxEvent
{
public:
    /**
        Constructor.
    */
    wxCalculateLayoutEvent(wxWindowID id = 0);

    /**
        Returns the flags associated with this event. Not currently used.
    */
    int GetFlags() const;

    /**
        Before the event handler is entered, returns the remaining parent client area
        that the window could occupy.

        When the event handler returns, this should contain the remaining parent
        client rectangle, after the event handler has subtracted the area that its
        window occupies.
    */
    wxRect GetRect() const;

    /**
        Sets the flags associated with this event. Not currently used.
    */
    void SetFlags(int flags);

    /**
        Call this to specify the new remaining parent client area, after the space
        occupied by the window has been subtracted.
    */
    void SetRect(const wxRect& rect);
};

wxEventType wxEVT_CALCULATE_LAYOUT;

